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

val token_to_string : token -> string
val tokenize : string -> token list
