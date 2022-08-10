type token =
  (* { *)
  | OpenBrace
  (* } *)
  | CloseBrace
  (* ( *)
  | OpenParen
  (* ) *)
  | CloseParen
  (* ; *)
  | Semicolon
  (* int *)
  | IntKeyword
  (* return *)
  | ReturnKeyword
  (* integer literal *)
  | Int of int
  (* identifier *)
  | Id of string

let token_to_string = function
  | OpenBrace -> "{"
  | CloseBrace -> "}"
  | OpenParen -> "("
  | CloseParen -> ")"
  | Semicolon -> ";"
  | ReturnKeyword -> "RETURN"
  | IntKeyword -> "INT"
  | Int i -> Printf.sprintf "INT<%d>" i
  | Id str -> Printf.sprintf "ID<%s>" str

let token_length = function
  | OpenBrace -> 1
  | CloseBrace -> 1
  | OpenParen -> 1
  | CloseParen -> 1
  | Semicolon -> 1
  | ReturnKeyword -> 6
  | IntKeyword -> 3
  | Int i -> String.length (string_of_int i)
  | Id str -> String.length str

let get_int_or_id_token input =
  let int_regexp = Str.regexp "[0-9]+" in
  let id_regexp = Str.regexp "[A-Za-z]+" in
  if Str.string_match int_regexp input 0 then
    Int (int_of_string (Str.matched_string input))
  else if Str.string_match id_regexp input 0 then
    let matched_string = Str.matched_string input in
    match matched_string with
    | "return" -> ReturnKeyword
    | "int" -> IntKeyword
    | _ -> Id matched_string
  else failwith "Syntax error."

let tokenize input =
  let rec tokens sub_input =
    match Util.split sub_input with
    | [] -> []
    | " " :: rest | "\n" :: rest -> tokens (Util.join rest)
    | "{" :: rest -> OpenBrace :: tokens (Util.join rest)
    | "}" :: rest -> CloseBrace :: tokens (Util.join rest)
    | "(" :: rest -> OpenParen :: tokens (Util.join rest)
    | ")" :: rest -> CloseParen :: tokens (Util.join rest)
    | ";" :: rest -> Semicolon :: tokens (Util.join rest)
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
  | head :: [] -> "\"" ^ token_to_string head ^ "\""
  | head :: rest -> "\"" ^ token_to_string head ^ "\"" ^ ", " ^ inspect rest
