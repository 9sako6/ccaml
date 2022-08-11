let transpile ast =
  let generate_function_body = function
    | Ast.Return (Ast.Const n) :: _ ->
        Printf.sprintf "  movl $%d, %%eax" n ^ "\n" ^ Printf.sprintf "  ret"
    | _ -> failwith "Transpile error."
  in
  let generate_function = function
    | Ast.Function (Ast.Id name, statements) ->
        generate_function_body statements |> Printf.sprintf "%s:\n%s" name
  in
  match ast with
  | Ast.Program f ->
      generate_function f |> Printf.sprintf "  .globl  main\n%s\n"
