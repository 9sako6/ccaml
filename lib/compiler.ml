let compile file_name =
  let ast = Util.read file_name |> Lexer.tokenize |> Parser.parse in
  (* let asm = Assembly.transpile ast in *)
  print_endline (Ast.inspect ast)
