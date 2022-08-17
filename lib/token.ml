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
  (* && *)
  | And
  (* || *)
  | Or
  (* = *)
  | Equal
  (* == *)
  | EqualEqual
  (* != *)
  | NotEqual
  (* < *)
  | LessThan
  (* <= *)
  | LessThanOrEqual
  (* > *)
  | GreaterThan
  (* >= *)
  | GreaterThanOrEqual
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
  | And -> "&&"
  | Or -> "||"
  | Equal -> "="
  | EqualEqual -> "=="
  | NotEqual -> "!="
  | LessThan -> "<"
  | LessThanOrEqual -> "<="
  | GreaterThan -> ">"
  | GreaterThanOrEqual -> ">="
  | ReturnKeyword -> "RETURN"
  | IntKeyword -> "INT"
  | Int i -> Printf.sprintf "INT<%d>" i
  | Id str -> Printf.sprintf "ID<%s>" str
