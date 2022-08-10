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

let tokenize input =
  let int_regexp = Str.regexp "[0-9]+" in
  let id_regexp = Str.regexp "[A-Za-z]+" in
  let rec tokens sub_input =
    match Util.split sub_input with
    | [] -> []
    | " " :: rest | "\n" :: rest -> tokens (Util.join rest)
    | "{" :: rest -> OpenBrace :: tokens (Util.join rest)
    | "}" :: rest -> CloseBrace :: tokens (Util.join rest)
    | "(" :: rest -> OpenParen :: tokens (Util.join rest)
    | ")" :: rest -> CloseParen :: tokens (Util.join rest)
    | ";" :: rest -> Semicolon :: tokens (Util.join rest)
    | _ :: rest ->
        if Str.string_match int_regexp sub_input 0 then
          let int_token = Str.matched_string sub_input in
          let rest_sub_input =
            String.sub sub_input (String.length int_token)
              (String.length sub_input - String.length int_token)
          in
          Int (int_of_string int_token) :: tokens rest_sub_input
        else if Str.string_match id_regexp sub_input 0 then
          let id_token = Str.matched_string sub_input in
          let rest_sub_input =
            String.sub sub_input (String.length id_token)
              (String.length sub_input - String.length id_token)
          in
          Id id_token :: tokens rest_sub_input
        else (* ignore unknown string *)
          tokens (Util.join rest)
  in
  tokens input

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
