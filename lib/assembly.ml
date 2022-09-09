let transpile ast =
  let open Ast in
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
    | Var name -> (
        try
          let var = Var.find name context in
          print_asm (Printf.sprintf "  mov -%d(%%rbp), %%rax" var.offset)
        with Not_found ->
          failwith
            (Printf.sprintf "'%s' undeclared (first use in this function)." name)
        )
    | Assign (name, exp) -> (
        try
          let var = Var.find name context in
          generate_expression context exp;
          print_asm (Printf.sprintf "  mov %%rax, -%d(%%rbp)" var.offset)
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
    | Exp exp -> generate_expression context exp
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
    | Block block_items ->
        (* Add a new scope. *)
        let context = Var.make_new_scope context in
        generate_block_items context block_items
  and generate_block_items context = function
    | [] -> ()
    | Statement statement :: rest ->
        generate_statement context statement;
        generate_block_items context rest
    | Declaration declaration :: rest -> (
        match declaration with
        | Declare (name, exp_option) ->
            let size = 8 in
            let stack_index = Var.stack_index context - size in
            let context =
              try Var.declare name size context
              with Var.Redefinition ->
                failwith (Printf.sprintf "redefinition of '%s'." name)
            in
            let () =
              match exp_option with
              | None -> ()
              | Some exp -> (
                  match Var.find name context with
                  | { size; _ } ->
                      (* Reserve a space of the local variable *)
                      print_asm (Printf.sprintf "  sub $%d, %%rsp" size);
                      generate_expression context exp;
                      print_asm
                        (Printf.sprintf "  mov %%rax, %d(%%rbp)" stack_index))
            in
            generate_block_items context rest)
  in
  let generate_function_def = function
    | Function (Id name, block_items) ->
        print_asm (Printf.sprintf "%s:" name);
        print_asm "  push %rbp";
        print_asm "  movq %rsp, %rbp";
        let context = Var.empty in
        generate_block_items context block_items
  in
  match ast with
  | Program fun_ast ->
      print_asm ".globl  main";
      generate_function_def fun_ast;
      !s
