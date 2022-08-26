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
    (* TODO: Fix *)
    | _ -> ()
  in
  let rec generate_expression var_map = function
    | Var name ->
        let offset = Var.find_offset name var_map in
        print_asm (Printf.sprintf "  mov -%d(%%rbp), %%rax" offset)
    | Assign (name, exp) ->
        let offset = Var.find_offset name var_map in
        generate_expression var_map exp;
        print_asm (Printf.sprintf "  mov %%rax, -%d(%%rbp)" offset)
    | Const n -> print_asm (Printf.sprintf "  mov $%d, %%rax" n)
    | UnaryOp (Negate, exp) ->
        generate_expression var_map exp;
        print_asm "  neg %rax"
    | UnaryOp (Complement, exp) ->
        generate_expression var_map exp;
        print_asm "  not %rax"
    | UnaryOp (Not, exp) ->
        generate_expression var_map exp;
        print_asm "  cmp $0, %rax";
        (* AL is a special flag register that holds the comparison result.
           `sete` sets AL register to 1 if EAX was 0 in earlier comparison. *)
        print_asm "  sete %al";
        print_asm "  movzb %al, %rax"
    | BinaryOp (And, left, right) ->
        let clause_id = Util.unique_id () in
        let right_label = Printf.sprintf "right_of_and_clause_%d" clause_id in
        let goal_label = Printf.sprintf "goal_of_and_clause_%d" clause_id in
        generate_expression var_map left;
        (* If left is not false, go to right's label. *)
        print_asm "  cmp $0, %rax";
        print_asm (Printf.sprintf "  jne %s" right_label);
        print_asm (Printf.sprintf "  jmp %s" goal_label);
        (* Emit label for right. *)
        print_asm (Printf.sprintf "%s:" right_label);
        generate_expression var_map right;
        print_asm "  cmp $0, %rax";
        print_asm "  setne %al";
        print_asm "  movzb %al, %rax";
        (* Emit label for goal. *)
        print_asm (Printf.sprintf "%s:" goal_label)
    | BinaryOp (Or, left, right) ->
        let clause_id = Util.unique_id () in
        let right_label = Printf.sprintf "right_of_or_clause_%d" clause_id in
        let goal_label = Printf.sprintf "goal_of_or_clause_%d" clause_id in
        generate_expression var_map left;
        (* If left is false, go to right's label. *)
        print_asm "  cmp $0, %rax";
        print_asm (Printf.sprintf "  je %s" right_label);
        (* left is true. *)
        print_asm "  mov $1, %rax";
        print_asm (Printf.sprintf "  jmp %s" goal_label);
        (* Emit label for right. *)
        print_asm (Printf.sprintf "%s:" right_label);
        generate_expression var_map right;
        print_asm "  cmp $0, %rax";
        print_asm "  setne %al";
        print_asm "  movzb %al, %rax";
        (* Emit label for goal. *)
        print_asm (Printf.sprintf "%s:" goal_label)
    | BinaryOp (op, left, right) ->
        generate_expression var_map left;
        (* Save the result of left expression. *)
        print_asm "  pushq %rax";
        generate_expression var_map right;
        print_asm "  mov %rax, %rcx";
        (* Get the result of left expression. *)
        print_asm "  popq %rax";
        generate_binary_operation op
  in
  let rec generate_function_body var_map = function
    | [] -> ()
    | Return exp :: rest ->
        generate_expression var_map exp;
        print_asm "  mov %rbp, %rsp";
        print_asm "  pop %rbp";
        print_asm "  ret";
        generate_function_body var_map rest;
        ()
    | Exp exp :: rest ->
        generate_expression var_map exp;
        generate_function_body var_map rest
    | Declare (name, exp_option) :: rest ->
        (* TODO: Fix for data size *)
        let size = 8 in
        let var_map = Var.add name size var_map in
        let () =
          match exp_option with
          | None -> ()
          | Some exp -> (
              match Var.find name var_map with
              | offset, size ->
                  (* Reserve a space of the local variable *)
                  print_asm (Printf.sprintf "  sub $%d, %%rsp" size);
                  generate_expression var_map exp;
                  print_asm (Printf.sprintf "  mov %%rax, -%d(%%rbp)" offset))
        in
        generate_function_body var_map rest
  and generate_function_def = function
    | Function (Id name, statements) ->
        print_asm (Printf.sprintf "%s:" name);
        print_asm "  push %rbp";
        print_asm "  movq %rsp, %rbp";
        let var_map = Var.empty in
        generate_function_body var_map statements
  in
  match ast with
  | Program fun_ast ->
      print_asm ".globl  main";
      generate_function_def fun_ast;
      !s
