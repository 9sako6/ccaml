let rec parse_expression tokens =
  let open Token in
  let construct_binary_op_exp op l r =
    match op with
    | Plus -> Ast.BinaryOp (Ast.Add, l, r)
    | Minus -> Ast.BinaryOp (Ast.Sub, l, r)
    | Asterisk -> Ast.BinaryOp (Ast.Mult, l, r)
    | Slash -> Ast.BinaryOp (Ast.Div, l, r)
    | And -> Ast.BinaryOp (Ast.And, l, r)
    | Or -> Ast.BinaryOp (Ast.Or, l, r)
    | EqualEqual -> Ast.BinaryOp (Ast.Equal, l, r)
    | NotEqual -> Ast.BinaryOp (Ast.NotEqual, l, r)
    | LessThan -> Ast.BinaryOp (Ast.LessThan, l, r)
    | LessThanOrEqual -> Ast.BinaryOp (Ast.LessThanOrEqual, l, r)
    | GreaterThan -> Ast.BinaryOp (Ast.GreaterThan, l, r)
    | GreaterThanOrEqual -> Ast.BinaryOp (Ast.GreaterThanOrEqual, l, r)
    | _ -> failwith "Parse error. Invalid binary operator."
  in

  let parse_binary_op_exp parse_next operators tokens =
    (* Join expressions of the same precedence for left-associative *)
    let left_exp, rest = parse_next tokens in
    let rec construct_exp left_exp tokens =
      let operator = List.hd tokens in
      if List.mem operator operators then
        (* left-associative *)
        let right_exp, rest = parse_next (List.tl tokens) in
        let left_exp = construct_binary_op_exp operator left_exp right_exp in
        construct_exp left_exp rest
      else (left_exp, tokens)
    in
    construct_exp left_exp rest
  in

  let rec parse_factor =
    let open Token in
    function
    (* "(" <exp> ")" *)
    | OpenParen :: factor -> (
        let exp, after_exp = parse_expression factor in
        match after_exp with
        | CloseParen :: rest -> (exp, rest)
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
    | Id name :: rest -> (Ast.Var name, rest)
    | _ -> failwith "Parse error. This is an invalid factor."
  in

  let parse_mult_div_exp =
    parse_binary_op_exp parse_factor [ Asterisk; Slash ]
  in

  let parse_add_sub_exp =
    parse_binary_op_exp parse_mult_div_exp [ Plus; Minus ]
  in

  let parse_comparison_exp =
    parse_binary_op_exp parse_add_sub_exp
      [ LessThan; LessThanOrEqual; GreaterThan; GreaterThanOrEqual ]
  in

  let parse_equality_exp =
    parse_binary_op_exp parse_comparison_exp [ EqualEqual; NotEqual ]
  in

  let parse_and_exp = parse_binary_op_exp parse_equality_exp [ And ] in

  let parse_or_exp = parse_binary_op_exp parse_and_exp [ Or ] in

  let rec parse_exp = function
    | Id name :: Equal :: rest ->
        let exp, sub_rest = parse_exp rest in
        (Ast.Assign (name, exp), sub_rest)
    | tokens -> parse_or_exp tokens
  in

  parse_exp tokens

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
    | IntKeyword :: Id name :: Equal :: rest ->
        let expression, rest = parse_expression rest in
        let other_statements, rest = partition rest in
        (Ast.Declare (name, Some expression) :: other_statements, rest)
    | (Id _name as var) :: Equal :: rest ->
        let expression, rest = parse_expression (var :: Equal :: rest) in
        let other_statements, rest = partition rest in
        (Ast.Exp expression :: other_statements, rest)
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
