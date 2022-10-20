exception Unknown_character of string

let token_length =
  let open Token in
  function
  | OpenBrace
  | CloseBrace
  | OpenParen
  | CloseParen
  | Colon
  | Semicolon
  | Equal
  | Minus
  | Plus
  | Asterisk
  | Slash
  | Tilde
  | LessThan
  | GreaterThan
  | Exclamation
  | Question
  | Comma -> 1
  | And | Or | EqualEqual | GreaterThanOrEqual | LessThanOrEqual | NotEqual -> 2
  | ReturnKeyword -> 6
  | IntKeyword -> 3
  | IfKeyword -> 2
  | ElseKeyword -> 4
  | ForKeyword -> 3
  | BreakKeyword -> 5
  | ContinueKeyword -> 8
  | Int i -> String.length (string_of_int i)
  | Id str -> String.length str

let get_int_or_id_token input =
  let open Token in
  let int_regexp = Str.regexp "[0-9]+" in
  let id_regexp = Str.regexp "[A-Za-z]+" in
  if Str.string_match int_regexp input 0 then
    Int (int_of_string (Str.matched_string input))
  else if Str.string_match id_regexp input 0 then
    let matched_string = Str.matched_string input in
    match matched_string with
    | "return" -> ReturnKeyword
    | "int" -> IntKeyword
    | "if" -> IfKeyword
    | "else" -> ElseKeyword
    | "for" -> ForKeyword
    | "break" -> BreakKeyword
    | "continue" -> ContinueKeyword
    | _ -> Id matched_string
  else
    raise
      (Unknown_character (Printf.sprintf "Failed to tokenize `%c`." input.[0]))

let tokenize input =
  let open Token in
  let rec tokens chars =
    match chars with
    | [] -> []
    | ' ' :: rest | '\n' :: rest -> tokens rest
    | '{' :: rest -> OpenBrace :: tokens rest
    | '}' :: rest -> CloseBrace :: tokens rest
    | '(' :: rest -> OpenParen :: tokens rest
    | ')' :: rest -> CloseParen :: tokens rest
    | ';' :: rest -> Semicolon :: tokens rest
    | '+' :: rest -> Plus :: tokens rest
    | '-' :: rest -> Minus :: tokens rest
    | '*' :: rest -> Asterisk :: tokens rest
    | '/' :: rest -> Slash :: tokens rest
    | '~' :: rest -> Tilde :: tokens rest
    | '?' :: rest -> Question :: tokens rest
    | ':' :: rest -> Colon :: tokens rest
    | '!' :: '=' :: rest -> NotEqual :: tokens rest
    | '!' :: rest -> Exclamation :: tokens rest
    | '&' :: '&' :: rest -> And :: tokens rest
    | '|' :: '|' :: rest -> Or :: tokens rest
    | '=' :: '=' :: rest -> EqualEqual :: tokens rest
    | '=' :: rest -> Equal :: tokens rest
    | '<' :: '=' :: rest -> LessThanOrEqual :: tokens rest
    | '<' :: rest -> LessThan :: tokens rest
    | '>' :: '=' :: rest -> GreaterThanOrEqual :: tokens rest
    | '>' :: rest -> GreaterThan :: tokens rest
    | ',' :: rest -> Comma :: tokens rest
    | _ :: _ ->
        let sub_input = String_util.implode chars in
        let token = get_int_or_id_token sub_input in
        let token_length = token_length token in
        let rest_sub_input =
          String.sub sub_input token_length
            (String.length sub_input - token_length)
        in
        token :: tokens (String_util.explode rest_sub_input)
  in
  tokens (String_util.explode input)

let rec inspect tokens =
  match tokens with
  | [] -> ""
  | head :: [] -> "\"" ^ Token.to_string head ^ "\""
  | head :: rest -> "\"" ^ Token.to_string head ^ "\"" ^ ", " ^ inspect rest
