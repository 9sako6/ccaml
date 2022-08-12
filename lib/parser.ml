let parse_statements tokens =
  (* Split the statements and the rest *)
  let rec partition sub_tokens =
    match sub_tokens with
    | Lexer.Semicolon :: rest -> partition rest
    | Lexer.ReturnKeyword :: Lexer.Int n :: rest ->
        let other_statements, sub_rest = partition rest in
        (Ast.Return (Ast.Const n) :: other_statements, sub_rest)
    | Lexer.ReturnKeyword :: rest ->
        let other_statements, sub_rest = partition rest in
        (* TODO: Is ok `return`'s default value 0 *)
        (Ast.Return (Ast.Const 0) :: other_statements, sub_rest)
    | _ -> ([], sub_tokens)
  in
  let statements, rest = partition tokens in
  match rest with
  | Lexer.CloseBrace :: [] -> statements
  | _ -> failwith "Parse error. `}` is missing."

let parse_function_def tokens =
  match tokens with
  | Lexer.IntKeyword
    :: Lexer.Id name
    :: Lexer.OpenParen :: Lexer.CloseParen :: Lexer.OpenBrace :: rest ->
      Ast.Function (Ast.Id name, parse_statements rest)
  | _ -> failwith "Parse error in a function." (* TODO: Kind error message *)

let parse tokens = Ast.Program (parse_function_def tokens)
