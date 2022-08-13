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
  (* + *)
  | Plus
  (* - *)
  | Minus
  (* * *)
  | Asterisk
  (* / *)
  | Slash
  (* ~ *)
  | Tilde
  (* ! *)
  | Exclamation
  (* int *)
  | IntKeyword
  (* return *)
  | ReturnKeyword
  (* integer literal *)
  | Int of int
  (* identifier *)
  | Id of string

let to_string = function
  | OpenBrace -> "{"
  | CloseBrace -> "}"
  | OpenParen -> "("
  | CloseParen -> ")"
  | Semicolon -> ";"
  | Minus -> "-"
  | Plus -> "+"
  | Asterisk -> "*"
  | Slash -> "/"
  | Tilde -> "~"
  | Exclamation -> "!"
  | ReturnKeyword -> "RETURN"
  | IntKeyword -> "INT"
  | Int i -> Printf.sprintf "INT<%d>" i
  | Id str -> Printf.sprintf "ID<%s>" str
