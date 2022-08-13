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
  in
  let rec generate_expression = function
    | Const n -> print_asm (Printf.sprintf "  mov $%d, %%rax" n)
    | UnaryOp (Negate, exp) ->
        generate_expression exp;
        print_asm "  neg %rax"
    | UnaryOp (Complement, exp) ->
        generate_expression exp;
        print_asm "  not %rax"
    | UnaryOp (Not, exp) ->
        generate_expression exp;
        print_asm "  cmp $0, %rax";
        (* AL is a special flag register that holds the comparison result.
           `sete` sets AL register to 1 if EAX was 0 in earlier comparison. *)
        print_asm "  sete %al";
        print_asm "  movzb %al, %rax"
    | BinaryOp (op, left, right) ->
        generate_expression left;
        (* Save the result of left expression. *)
        print_asm "  pushq %rax";
        generate_expression right;
        print_asm "  mov %rax, %rcx";
        (* Get the result of left expression. *)
        print_asm "  popq %rax";
        generate_binary_operation op
  and generate_function_body = function
    | Return exp :: _ ->
        generate_expression exp;
        print_asm "  ret"
    | _ -> failwith "Transpile error."
  and generate_function_def = function
    | Function (Id name, statements) ->
        print_asm (Printf.sprintf "%s:" name);
        generate_function_body statements
  in
  match ast with
  | Program fun_ast ->
      print_asm ".globl  main";
      generate_function_def fun_ast;
      !s
