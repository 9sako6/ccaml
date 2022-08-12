let rec parse_expression tokens =
  let open Token in
  match tokens with
  | Minus :: rest ->
      let exp, sub_rest = parse_expression rest in
      (Ast.UnaryOp (Ast.Negate, exp), sub_rest)
  | Exclamation :: rest ->
      let exp, sub_rest = parse_expression rest in
      (Ast.UnaryOp (Ast.Not, exp), sub_rest)
  | Tilde :: rest ->
      let exp, sub_rest = parse_expression rest in
      (Ast.UnaryOp (Ast.Complement, exp), sub_rest)
  | Int n :: rest -> (Ast.Const n, rest)
  | _ -> failwith "Parse error. Invalid operator."

let parse_statements tokens =
  let open Token in
  (* Split the statements and the rest *)
  let rec partition sub_tokens =
    match sub_tokens with
    | Semicolon :: rest -> partition rest
    | ReturnKeyword :: Semicolon :: _ ->
        failwith "Parse error. `return` returns empty."
    | ReturnKeyword :: rest ->
        let expression, rest = parse_expression rest in
        let other_statements, rest = partition rest in
        (Ast.Return expression :: other_statements, rest)
    | _ -> ([], sub_tokens)
  in
  let statements, rest = partition tokens in
  match rest with
  | CloseBrace :: [] -> statements
  | _ -> failwith "Parse error. `}` is missing."

let parse_function_def tokens =
  let open Token in
  match tokens with
  | IntKeyword :: Id name :: OpenParen :: CloseParen :: OpenBrace :: rest ->
      Ast.Function (Ast.Id name, parse_statements rest)
  | _ -> failwith "Parse error in a function." (* TODO: Kind error message *)

let parse tokens = Ast.Program (parse_function_def tokens)
