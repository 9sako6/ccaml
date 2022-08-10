let rec parse_statement_list tokens =
  match tokens with
  | Lexer.Semicolon :: rest -> parse_statement_list rest
  | Lexer.ReturnKeyword :: Lexer.Int n :: rest ->
      let other_statements, r = parse_statement_list rest in
      (Ast.Return (Ast.Const n) :: other_statements, r)
  | Lexer.ReturnKeyword :: rest ->
      let other_statements, r = parse_statement_list rest in
      (* TODO: Is ok `return`'s default value 0 *)
      (Ast.Return (Ast.Const 0) :: other_statements, r)
  | _ -> ([], tokens)

let parse_statement tokens =
  let statements, rest = parse_statement_list tokens in
  match rest with
  | Lexer.CloseBrace :: [] -> statements
  | _ -> failwith "Parse error. `}` is missing."

let parse_function_def tokens =
  match tokens with
  | Lexer.IntKeyword
    :: Lexer.Id name
    :: Lexer.OpenParen :: Lexer.CloseParen :: Lexer.OpenBrace :: rest ->
      Ast.Function (Ast.Id name, parse_statement rest)
  | _ -> failwith "Parse error in a function." (* TODO: Kind error message *)

let parse tokens = Ast.Program (parse_function_def tokens)
