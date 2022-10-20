open Ast

(* Validate that AST has valid function declarations and definitions. *)
let validate ast =
  let module FunctionMap = Map.Make (String) in
  let find_definition name func_map =
    try Some (FunctionMap.find (name ^ " definition") func_map)
    with Not_found -> None
  in
  let find_declaration name func_map =
    try Some (FunctionMap.find (name ^ " declaration") func_map)
    with Not_found -> None
  in
  let validate existing_def incomming_def =
    let validate_params existing_params incomming_params =
      if List.length existing_params == List.length incomming_params then true
      else failwith "number of arguments doesn't match prototype."
    in
    let validate_body name existing_body incomming_body =
      match (existing_body, incomming_body) with
      | None, None -> true
      | None, Some _ -> true
      | Some _, None -> true
      | Some _, Some _ -> failwith (Printf.sprintf "redefinition of '%s'." name)
    in
    match (existing_def, incomming_def) with
    | ( Function { name; params; body },
        Function { name = _new_name; params = new_params; body = new_body } ) ->
        validate_params params new_params && validate_body name body new_body
  in
  let rec validate_functions function_def_list func_map =
    match function_def_list with
    | [] -> true
    | (Function { name; body; _ } as incomming_func_def_or_decl) :: rest -> (
        let label =
          name
          ^
          match body with
          | Some _ -> " definition"
          | None -> " declaration"
        in
        let existing_func_def_option = find_definition name func_map in
        let existing_func_decl_option = find_declaration name func_map in
        match (existing_func_def_option, existing_func_decl_option) with
        | None, None ->
            (* First declaration or definition *)
            let func_map =
              FunctionMap.add label incomming_func_def_or_decl func_map
            in
            validate_functions rest func_map
        | None, Some existing_func_decl ->
            let func_map =
              FunctionMap.add label incomming_func_def_or_decl func_map
            in
            if validate existing_func_decl incomming_func_def_or_decl then
              validate_functions rest func_map
            else false
        | Some existing_func_def, None ->
            let func_map =
              FunctionMap.add label incomming_func_def_or_decl func_map
            in
            if validate existing_func_def incomming_func_def_or_decl then
              validate_functions rest func_map
            else false
        | Some existing_func_def, Some existing_func_decl ->
            if
              validate existing_func_def incomming_func_def_or_decl
              && validate existing_func_decl incomming_func_def_or_decl
            then validate_functions rest func_map
            else false)
  in
  match ast with
  | Program function_def_list ->
      let func_map = FunctionMap.empty in
      validate_functions function_def_list func_map

let transpile ast =
  let () =
    if validate ast then ()
    else failwith "invalid function definitions or declarations."
  in
  let s = ref "" in
  let print_asm row = s := !s ^ row ^ "\n" in
  let generate_binary_operation = function
    | Add -> print_asm "  add %rcx, %rax"
    | Sub -> print_asm "  sub %rcx, %rax"
    | Mult -> print_asm "  imul %rcx, %rax"
    | Div ->
        print_asm "  cqo";
        print_asm "  idiv %rcx"
    | Equal ->
        print_asm "  cmp %rcx, %rax";
        print_asm "  sete %al";
        print_asm "  movzb %al, %rax"
    | NotEqual ->
        print_asm "  cmp %rcx, %rax";
        print_asm "  setne %al";
        print_asm "  movzb %al, %rax"
    | LessThan ->
        print_asm "  cmp %rcx, %rax";
        print_asm "  setl %al";
        print_asm "  movzb %al, %rax"
    | LessThanOrEqual ->
        print_asm "  cmp %rcx, %rax";
        print_asm "  setle %al";
        print_asm "  movzb %al, %rax"
    | GreaterThan ->
        print_asm "  cmp %rcx, %rax";
        print_asm "  setg %al";
        print_asm "  movzb %al, %rax"
    | GreaterThanOrEqual ->
        print_asm "  cmp %rcx, %rax";
        print_asm "  setge %al";
        print_asm "  movzb %al, %rax"
    | _ -> failwith "Unknown binary operator."
  in
  let rec generate_expression context = function
    | FunCall (name, params) ->
        (* Push params from right to left. *)
        let rec generate_params index = function
          | [] -> ()
          | head :: rest ->
              generate_expression context head;
              (*
                 Copy a parameter to a register specified by System V AMD64 ABI.
                 However, no registers are used in my functions.
                 These registers are intended to be used in standard library functions.
              *)
              print_asm
                (Printf.sprintf "  mov %%rax, %%%s"
                   (Array.get Var.registers index));
              print_asm "  pushq %rax";
              generate_params index rest
        in
        generate_params (List.length params - 1) (List.rev params);
        (* Call function. *)
        print_asm (Printf.sprintf "  call %s" name);
        (* Clear params. *)
        print_asm (Printf.sprintf "  add $%d, %%rsp" (8 * List.length params))
    | Var name -> (
        try
          let var = Var.find name context in
          print_asm (Printf.sprintf "  mov %d(%%rbp), %%rax" var.index)
        with Not_found ->
          failwith
            (Printf.sprintf "'%s' undeclared (first use in this function)." name)
        )
    | Assign (name, exp) -> (
        try
          let var = Var.find name context in
          generate_expression context exp;
          print_asm (Printf.sprintf "  mov %%rax, %d(%%rbp)" var.index)
        with Not_found ->
          failwith
            (Printf.sprintf "'%s' undeclared (first use in this function)." name)
        )
    | Const n -> print_asm (Printf.sprintf "  mov $%d, %%rax" n)
    | UnaryOp (Negate, exp) ->
        generate_expression context exp;
        print_asm "  neg %rax"
    | UnaryOp (Complement, exp) ->
        generate_expression context exp;
        print_asm "  not %rax"
    | UnaryOp (Not, exp) ->
        generate_expression context exp;
        print_asm "  cmp $0, %rax";
        (* AL is a special flag register that holds the comparison result.
           `sete` sets AL register to 1 if EAX was 0 in earlier comparison. *)
        print_asm "  sete %al";
        print_asm "  movzb %al, %rax"
    | BinaryOp (And, left, right) ->
        let clause_id = Util.unique_id () in
        let right_label = Printf.sprintf "right_of_and_clause_%d" clause_id in
        let goal_label = Printf.sprintf "goal_of_and_clause_%d" clause_id in
        generate_expression context left;
        (* If left is not false, go to right's label. *)
        print_asm "  cmp $0, %rax";
        print_asm (Printf.sprintf "  jne %s" right_label);
        print_asm (Printf.sprintf "  jmp %s" goal_label);
        (* Emit label for right. *)
        print_asm (Printf.sprintf "%s:" right_label);
        generate_expression context right;
        print_asm "  cmp $0, %rax";
        print_asm "  setne %al";
        print_asm "  movzb %al, %rax";
        (* Emit label for goal. *)
        print_asm (Printf.sprintf "%s:" goal_label)
    | BinaryOp (Or, left, right) ->
        let clause_id = Util.unique_id () in
        let right_label = Printf.sprintf "right_of_or_clause_%d" clause_id in
        let goal_label = Printf.sprintf "goal_of_or_clause_%d" clause_id in
        generate_expression context left;
        (* If left is false, go to right's label. *)
        print_asm "  cmp $0, %rax";
        print_asm (Printf.sprintf "  je %s" right_label);
        (* left is true. *)
        print_asm "  mov $1, %rax";
        print_asm (Printf.sprintf "  jmp %s" goal_label);
        (* Emit label for right. *)
        print_asm (Printf.sprintf "%s:" right_label);
        generate_expression context right;
        print_asm "  cmp $0, %rax";
        print_asm "  setne %al";
        print_asm "  movzb %al, %rax";
        (* Emit label for goal. *)
        print_asm (Printf.sprintf "%s:" goal_label)
    | BinaryOp (op, left, right) ->
        generate_expression context left;
        (* Save the result of left expression. *)
        print_asm "  pushq %rax";
        generate_expression context right;
        print_asm "  mov %rax, %rcx";
        (* Get the result of left expression. *)
        print_asm "  popq %rax";
        generate_binary_operation op
    | Condition (exp, exp_if, exp_else) ->
        let clause_id = Util.unique_id () in
        let else_label = Printf.sprintf "ternary_else_%d" clause_id in
        let goal_label = Printf.sprintf "ternary_after_else_%d" clause_id in
        generate_expression context exp;
        print_asm "  cmp $0, %rax";
        print_asm (Printf.sprintf "  je %s" else_label);
        generate_expression context exp_if;
        print_asm (Printf.sprintf "  jmp %s" goal_label);
        print_asm (Printf.sprintf "%s:" else_label);
        generate_expression context exp_else;
        print_asm (Printf.sprintf "%s:" goal_label)
  in
  let rec generate_statement context statement =
    match statement with
    | Return exp ->
        generate_expression context exp;
        print_asm "  mov %rbp, %rsp";
        print_asm "  pop %rbp";
        print_asm "  ret"
    | Exp exp_option -> (
        match exp_option with
        | None -> ()
        | Some exp -> generate_expression context exp)
    | If (exp, if_statement, statement_option) -> (
        let clause_id = Util.unique_id () in
        let else_label = Printf.sprintf "else_%d" clause_id in
        let goal_label = Printf.sprintf "after_else_%d" clause_id in
        generate_expression context exp;
        print_asm "  cmp $0, %rax";
        match statement_option with
        | Some else_statement ->
            print_asm (Printf.sprintf "  je %s" else_label);
            generate_statement context if_statement;
            print_asm (Printf.sprintf "  jmp %s" goal_label);
            print_asm (Printf.sprintf "%s:" else_label);
            generate_statement context else_statement;
            print_asm (Printf.sprintf "%s:" goal_label)
        | None ->
            print_asm (Printf.sprintf "  je %s" goal_label);
            generate_statement context if_statement;
            print_asm (Printf.sprintf "  jmp %s" goal_label);
            print_asm (Printf.sprintf "%s:" goal_label))
    | For _ | ForDecl _ -> generate_for_statement context statement
    | Block block_items ->
        (* Add a new scope. *)
        let context = Var.make_new_scope context in
        generate_block_items context block_items
    | Break -> (
        match context.break_label with
        | None -> failwith "A label for `break` is not found"
        | Some label -> print_asm (Printf.sprintf "  jmp %s" label))
    | Continue -> (
        match context.continue_label with
        | None -> failwith "A label for `continue` is not found"
        | Some label -> print_asm (Printf.sprintf "  jmp %s" label))
  and generate_for_statement context statement =
    let condition_label = Printf.sprintf "L%d" (Util.unique_id ()) in
    let continue_label = Printf.sprintf "L%d" (Util.unique_id ()) in
    let goal_label = Printf.sprintf "L%d" (Util.unique_id ()) in
    let context, condition_exp, post_exp_option, statement =
      match statement with
      | For (init_exp_option, condition_exp, post_exp_option, statement) ->
          (* Evaluate initial expression. *)
          (match init_exp_option with
          | None -> ()
          | Some exp -> generate_expression context exp);
          (context, condition_exp, post_exp_option, statement)
      | ForDecl (declaration, condition_exp, post_exp_option, statement) ->
          (* Evaluate declaration. *)
          let context = generate_declaration context declaration in
          (context, condition_exp, post_exp_option, statement)
      | _ -> failwith "Invalid `for` statement."
    in
    (* Set label for break. *)
    let context = Var.set_berak_label goal_label context in
    (* Set label for continue *)
    let context = Var.set_continue_label continue_label context in
    (* Evaluate condition. If it's false, it's done. *)
    print_asm (Printf.sprintf "%s:" condition_label);
    generate_expression context condition_exp;
    print_asm "  cmp $0, %rax";
    print_asm (Printf.sprintf "  je %s" goal_label);
    generate_statement context statement;
    (* Evaluate post expresssion. *)
    print_asm (Printf.sprintf "%s:" continue_label);
    (match post_exp_option with
    | None -> ()
    | Some exp -> generate_expression context exp);
    print_asm (Printf.sprintf "  jmp %s" condition_label);
    print_asm (Printf.sprintf "%s:" goal_label)
  and generate_declaration context declaration =
    match declaration with
    | Declare (name, exp_option) -> (
        let size = 8 in
        (* let stack_index = Var.stack_index context - size in *)
        let context =
          try Var.declare name size context
          with Var.Redefinition ->
            failwith (Printf.sprintf "redefinition of '%s'." name)
        in
        match Var.find name context with
        | { size; _ } ->
            (* Reserve a space of the local variable *)
            print_asm (Printf.sprintf "  sub $%d, %%rsp" size);
            (match exp_option with
            | None -> ()
            | Some exp -> generate_expression context exp);
            print_asm
              (Printf.sprintf "  mov %%rax, %d(%%rbp)" context.stack_index);
            context)
  and generate_block_items context = function
    | [] -> ()
    | Statement statement :: rest ->
        generate_statement context statement;
        generate_block_items context rest
    | Declaration declaration :: rest ->
        let context = generate_declaration context declaration in
        generate_block_items context rest
  in
  let generate_function = function
    | Function { name; body = block_items_option; params } -> (
        match block_items_option with
        | None ->
            (* This is a function declaration. *)
            ()
        | Some block_items ->
            print_asm (Printf.sprintf "  .global %s" name);
            print_asm (Printf.sprintf "%s:" name);
            print_asm "  push %rbp";
            print_asm "  movq %rsp, %rbp";
            let context = Var.empty in
            let size = 8 in
            let rec declare_params params index context =
              match params with
              | [] -> context
              | name :: rest ->
                  let index = index + size in
                  let context = Var.add_param name size index context in
                  declare_params rest index context
            in
            (* Skip spaces used by RBP and `call` mnemonic. *)
            let index = 8 in
            let context =
              try declare_params params index context
              with Var.Redefinition ->
                failwith (Printf.sprintf "redefinition of '%s'." name)
            in
            generate_block_items context block_items)
  in
  let generate_functions function_def_list =
    List.iter generate_function function_def_list
  in
  match ast with
  | Program function_def_list ->
      let () = generate_functions function_def_list in
      !s
