let rec parse_expression tokens =
  let open Token in
  let rec parse_factor =
    let open Token in
    function
    (* "(" <exp> ")" *)
    | OpenParen :: factor -> (
        let exp, rest = parse_expression factor in
        match rest with
        | CloseParen :: sub_rest -> (exp, sub_rest)
        | _ -> failwith "Parse error. `)` is missing.")
    (* <unary_op> <factor> *)
    | Minus :: factor ->
        let exp, rest = parse_factor factor in
        (Ast.UnaryOp (Ast.Negate, exp), rest)
    | Tilde :: factor ->
        let exp, rest = parse_factor factor in
        (Ast.UnaryOp (Ast.Complement, exp), rest)
    | Exclamation :: factor ->
        let exp, rest = parse_factor factor in
        (Ast.UnaryOp (Ast.Not, exp), rest)
    (* <int> *)
    | Int n :: rest -> (Ast.Const n, rest)
    | _ -> failwith "Parse error. This is an invalid factor."
  and parse_term ts =
    let open Token in
    let factor, after_factor = parse_factor ts in
    match after_factor with
    | Asterisk :: rest ->
        let next_factor, after_next_factor = parse_factor rest in
        (Ast.BinaryOp (Ast.Mult, factor, next_factor), after_next_factor)
    | Slash :: rest ->
        let next_factor, after_next_factor = parse_factor rest in
        (Ast.BinaryOp (Ast.Div, factor, next_factor), after_next_factor)
    | _ -> (factor, after_factor)
  in

  let term, after_term = parse_term tokens in
  match after_term with
  | Plus :: rest ->
      let next_term, after_next_term = parse_expression rest in
      (Ast.BinaryOp (Ast.Add, term, next_term), after_next_term)
  | Minus :: rest ->
      let next_term, after_next_term = parse_expression rest in
      (Ast.BinaryOp (Ast.Sub, term, next_term), after_next_term)
  | _ -> (term, after_term)

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
