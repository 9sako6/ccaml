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
  | Question -> 1
  | And | Or | EqualEqual | GreaterThanOrEqual | LessThanOrEqual | NotEqual -> 2
  | ReturnKeyword -> 6
  | IntKeyword -> 3
  | IfKeyword -> 2
  | ElseKeyword -> 4
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
    | _ -> Id matched_string
  else
    raise
      (Unknown_character (Printf.sprintf "Failed to tokenize `%c`." input.[0]))

let tokenize input =
  let open Token in
  let rec tokens sub_input =
    match Util.split sub_input with
    | [] -> []
    | " " :: rest | "\n" :: rest -> tokens (Util.join rest)
    | "{" :: rest -> OpenBrace :: tokens (Util.join rest)
    | "}" :: rest -> CloseBrace :: tokens (Util.join rest)
    | "(" :: rest -> OpenParen :: tokens (Util.join rest)
    | ")" :: rest -> CloseParen :: tokens (Util.join rest)
    | ";" :: rest -> Semicolon :: tokens (Util.join rest)
    | "+" :: rest -> Plus :: tokens (Util.join rest)
    | "-" :: rest -> Minus :: tokens (Util.join rest)
    | "*" :: rest -> Asterisk :: tokens (Util.join rest)
    | "/" :: rest -> Slash :: tokens (Util.join rest)
    | "~" :: rest -> Tilde :: tokens (Util.join rest)
    | "!" :: "=" :: rest -> NotEqual :: tokens (Util.join rest)
    | "!" :: rest -> Exclamation :: tokens (Util.join rest)
    | "&" :: "&" :: rest -> And :: tokens (Util.join rest)
    | "|" :: "|" :: rest -> Or :: tokens (Util.join rest)
    | "=" :: "=" :: rest -> EqualEqual :: tokens (Util.join rest)
    | "=" :: rest -> Equal :: tokens (Util.join rest)
    | "<" :: "=" :: rest -> LessThanOrEqual :: tokens (Util.join rest)
    | "<" :: rest -> LessThan :: tokens (Util.join rest)
    | ">" :: "=" :: rest -> GreaterThanOrEqual :: tokens (Util.join rest)
    | ">" :: rest -> GreaterThan :: tokens (Util.join rest)
    | _ :: _ ->
        let token = get_int_or_id_token sub_input in
        let token_length = token_length token in
        let rest_sub_input =
          String.sub sub_input token_length
            (String.length sub_input - token_length)
        in
        token :: tokens rest_sub_input
  in
  tokens input

let rec inspect tokens =
  match tokens with
  | [] -> ""
  | head :: [] -> "\"" ^ Token.to_string head ^ "\""
  | head :: rest -> "\"" ^ Token.to_string head ^ "\"" ^ ", " ^ inspect rest
