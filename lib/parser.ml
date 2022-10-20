open Token

let rec parse_expression tokens =
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
      match tokens with
      | [] -> (left_exp, tokens)
      | operator :: rest ->
          if List.mem operator operators then
            (* left-associative *)
            let right_exp, rest = parse_next rest in
            let left_exp =
              construct_binary_op_exp operator left_exp right_exp
            in
            construct_exp left_exp rest
          else (left_exp, tokens)
    in
    construct_exp left_exp rest
  in

  let parse_actual_params tokens =
    let rec split heads tails =
      match tails with
      | [] -> ([], [])
      | CloseParen :: rest -> (heads, rest)
      | head :: rest -> split (heads @ [ head ]) rest
    in
    let rec parse_params exps tokens_for_exp tails =
      match tails with
      | [] ->
          if List.length tokens_for_exp > 0 then
            let exp, _ = parse_expression tokens_for_exp in
            exps @ [ exp ]
          else exps
      | Comma :: rest ->
          let exp, _ = parse_expression tokens_for_exp in
          parse_params (exps @ [ exp ]) [] rest
      | head :: rest -> parse_params exps (tokens_for_exp @ [ head ]) rest
    in
    let params_tokens, rest = split [] tokens in
    let exps = parse_params [] [] params_tokens in
    (exps, rest)
  in

  let rec parse_factor = function
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
    | Id name :: OpenParen :: rest ->
        let params, rest = parse_actual_params rest in
        (Ast.FunCall (name, params), rest)
    | Id name :: rest -> (Ast.Var name, rest)
    | IfKeyword :: _ -> failwith "expected expression before 'if'."
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

  let rec parse_ternary_exp tokens =
    let exp, rest = parse_or_exp tokens in
    match rest with
    | Question :: rest -> (
        let exp2, rest = parse_exp rest in
        match rest with
        | Colon :: rest ->
            let exp3, rest = parse_ternary_exp rest in
            (Ast.Condition (exp, exp2, exp3), rest)
        | _ -> failwith "`:` is expected as a ternary operator.")
    | _ -> (exp, rest)
  and parse_exp = function
    | Id name :: Equal :: rest ->
        let exp, sub_rest = parse_exp rest in
        (Ast.Assign (name, exp), sub_rest)
    | tokens -> parse_ternary_exp tokens
  in

  parse_exp tokens

let rec parse_statement tokens =
  match tokens with
  | Semicolon :: rest -> parse_statement rest
  | ReturnKeyword :: Semicolon :: _ ->
      failwith "Parse error. `return` returns empty."
  | ReturnKeyword :: rest ->
      let expression, rest = parse_expression rest in
      (Ast.Return expression, rest)
  | (Id _name as var) :: Equal :: rest ->
      let expression, rest = parse_expression (var :: Equal :: rest) in
      (Ast.Exp (Some expression), rest)
  | IfKeyword :: rest -> (
      let expression, rest = parse_expression rest in
      let statement, rest = parse_statement rest in
      match rest with
      (* `if` statement with block statement. *)
      | ElseKeyword :: rest
      (* `if` statement with one line statement. *)
      | Semicolon :: ElseKeyword :: rest ->
          let statement_for_else, rest = parse_statement rest in
          (Ast.If (expression, statement, Some statement_for_else), rest)
      | _ -> (Ast.If (expression, statement, None), rest))
  | ForKeyword :: OpenParen :: rest ->
      parse_for_statement (ForKeyword :: OpenParen :: rest)
  | OpenBrace :: rest ->
      let split_block_tokens_and_others tokens =
        let rec split index score tails block_tokens =
          match tails with
          | [] -> failwith "fail to parse block."
          | head :: rest ->
              let new_score =
                if head == OpenBrace then score + 1
                else if head == CloseBrace then score - 1
                else score
              in
              if new_score == 0 then (block_tokens, rest)
              else split (index + 1) new_score rest (block_tokens @ [ head ])
        in
        split 0 1 tokens []
      in
      let block_tokens, rest = split_block_tokens_and_others rest in
      let block_items, _ = parse_block_items block_tokens in
      (Ast.Block block_items, rest)
  | BreakKeyword :: rest -> (Ast.Break, rest)
  | ContinueKeyword :: rest -> (Ast.Continue, rest)
  | IntKeyword :: _ -> failwith "expected expression before 'int'."
  | CloseBrace :: _ -> failwith "too many `}`."
  | _ ->
      let exp, rest = parse_expression tokens in
      (Ast.Exp (Some exp), rest)

and parse_for_statement tokens =
  match tokens with
  | ForKeyword :: OpenParen :: IntKeyword :: rest ->
      let declaration, rest = parse_declaration (IntKeyword :: rest) in
      let condition_expression, rest =
        match rest with
        | Semicolon :: rest -> parse_expression rest
        | _ -> failwith "expected `;`."
      in
      let post_expression, rest =
        match rest with
        | Semicolon :: rest -> parse_expression rest
        | _ -> failwith "expected `;`."
      in
      let statement, rest =
        match rest with
        | CloseParen :: rest -> parse_statement rest
        | _ -> failwith "expected `;`."
      in
      ( Ast.ForDecl
          (declaration, condition_expression, Some post_expression, statement),
        rest )
  | ForKeyword :: OpenParen :: rest ->
      let initial_expression, rest = parse_expression rest in
      let condition_expression, rest =
        match rest with
        | Semicolon :: rest -> parse_expression rest
        | _ -> failwith "expected `;`."
      in
      let post_expression, rest =
        match rest with
        | Semicolon :: rest -> parse_expression rest
        | _ -> failwith "expected `;`."
      in
      let statement, rest =
        match rest with
        | CloseParen :: rest -> parse_statement rest
        | _ -> failwith "expected `;`."
      in
      ( Ast.For
          ( Some initial_expression,
            condition_expression,
            Some post_expression,
            statement ),
        rest )
  | _ ->
      failwith
        (Printf.sprintf "Unexpected token `%s` for `for` statement."
           (Token.to_string (List.hd tokens)))

and parse_declaration = function
  | Semicolon :: rest -> parse_declaration rest
  | IntKeyword :: Id name :: Equal :: rest ->
      let expression, rest = parse_expression rest in
      (Ast.Declare (name, Some expression), rest)
  | IntKeyword :: Id name :: rest -> (Ast.Declare (name, None), rest)
  | _ -> failwith "Unknown token to parse a declaration."

and parse_block_items tokens =
  match tokens with
  | [] -> ([], [])
  | IntKeyword :: Id _ :: OpenParen :: _ ->
      (* function definition *)
      ([], tokens)
  | Semicolon :: rest -> parse_block_items rest
  | ReturnKeyword :: _
  | Id _ :: _
  | IfKeyword :: _
  | ForKeyword :: _
  | BreakKeyword :: _
  | ContinueKeyword :: _
  | OpenBrace :: _ ->
      let statement, rest = parse_statement tokens in
      let other_block_items, rest = parse_block_items rest in
      (Ast.Statement statement :: other_block_items, rest)
  | IntKeyword :: _ ->
      let declaration, rest = parse_declaration tokens in
      let other_block_items, rest = parse_block_items rest in
      (Ast.Declaration declaration :: other_block_items, rest)
  | ElseKeyword :: _ -> failwith "'else' without a previous 'if'."
  | _ ->
      let statement, rest = parse_statement tokens in
      let other_block_items, rest = parse_block_items rest in
      (Ast.Statement statement :: other_block_items, rest)

let parse_params tokens =
  let rec split heads tails =
    match tails with
    | [] -> ([], [])
    | CloseParen :: rest -> (heads, rest)
    | head :: rest -> (
        match head with
        | Id name -> split (heads @ [ name ]) rest
        | _ -> split heads rest)
  in
  let params_tokens, rest = split [] tokens in
  (params_tokens, rest)

let parse_function = function
  | IntKeyword :: Id name :: OpenParen :: rest ->
      let params_tokens, rest = parse_params rest in
      let block_items, rest = parse_block_items rest in
      ( Ast.Function
          {
            name;
            params = params_tokens;
            body = (if block_items == [] then None else Some block_items);
          },
        rest )
  | _ -> failwith "Parse error in a function."

let rec parse_functions tokens =
  match tokens with
  | [] -> ([], [])
  | IntKeyword :: Id _ :: OpenParen :: _ ->
      let func, rest = parse_function tokens in
      let ohter_funcs, rest = parse_functions rest in
      (func :: ohter_funcs, rest)
  | _ -> failwith "Parse error in parse_functions"

let parse tokens =
  let funcs, _rest = parse_functions tokens in
  Ast.Program funcs
