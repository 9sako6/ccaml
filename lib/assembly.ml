let transpile ast =
  let open Ast in
  let s = ref "" in
  let rec calc_expression = function
    | Const n -> n
    | UnaryOp (Negate, exp) -> -calc_expression exp
    | UnaryOp (Complement, exp) -> lnot (calc_expression exp)
    | UnaryOp (Not, exp) -> if 0 = calc_expression exp then 1 else 0
  and generate_expression exp =
    s := !s ^ Printf.sprintf "  movl $%d, %%eax\n" (calc_expression exp);
    ()
  and generate_function_body = function
    | Return exp :: _ ->
        generate_expression exp;
        s := !s ^ "  ret\n"
    | _ -> failwith "Transpile error."
  and generate_function_def = function
    | Function (Id name, statements) ->
        s := !s ^ Printf.sprintf "%s:\n" name;
        generate_function_body statements
  in
  match ast with
  | Program fun_ast ->
      s := !s ^ "  .globl  main\n";
      generate_function_def fun_ast;
      !s
