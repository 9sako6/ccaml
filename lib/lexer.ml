let token_length = function
  | Token.OpenBrace
  | Token.CloseBrace
  | Token.OpenParen
  | Token.CloseParen
  | Token.Semicolon
  | Token.Minus
  | Token.Tilde
  | Token.Exclamation -> 1
  | Token.ReturnKeyword -> 6
  | Token.IntKeyword -> 3
  | Token.Int i -> String.length (string_of_int i)
  | Token.Id str -> String.length str

let get_int_or_id_token input =
  let int_regexp = Str.regexp "[0-9]+" in
  let id_regexp = Str.regexp "[A-Za-z]+" in
  if Str.string_match int_regexp input 0 then
    Token.Int (int_of_string (Str.matched_string input))
  else if Str.string_match id_regexp input 0 then
    let matched_string = Str.matched_string input in
    match matched_string with
    | "return" -> Token.ReturnKeyword
    | "int" -> Token.IntKeyword
    | _ -> Token.Id matched_string
  else failwith "Syntax error."

let tokenize input =
  let rec tokens sub_input =
    match Util.split sub_input with
    | [] -> []
    | " " :: rest | "\n" :: rest -> tokens (Util.join rest)
    | "{" :: rest -> Token.OpenBrace :: tokens (Util.join rest)
    | "}" :: rest -> Token.CloseBrace :: tokens (Util.join rest)
    | "(" :: rest -> Token.OpenParen :: tokens (Util.join rest)
    | ")" :: rest -> Token.CloseParen :: tokens (Util.join rest)
    | ";" :: rest -> Token.Semicolon :: tokens (Util.join rest)
    | "-" :: rest -> Token.Minus :: tokens (Util.join rest)
    | "~" :: rest -> Token.Tilde :: tokens (Util.join rest)
    | "!" :: rest -> Token.Exclamation :: tokens (Util.join rest)
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
