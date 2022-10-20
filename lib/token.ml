type token =
  (* { *)
  | OpenBrace
  (* } *)
  | CloseBrace
  (* ( *)
  | OpenParen
  (* ) *)
  | CloseParen
  (* : *)
  | Colon
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
  (* ? *)
  | Question
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
  (* , *)
  | Comma
  (* int *)
  | IntKeyword
  (* return *)
  | ReturnKeyword
  | IfKeyword
  | ElseKeyword
  | ForKeyword
  | BreakKeyword
  | ContinueKeyword
  (* integer literal *)
  | Int of int
  (* identifier *)
  | Id of string

let to_string = function
  | OpenBrace -> "{"
  | CloseBrace -> "}"
  | OpenParen -> "("
  | CloseParen -> ")"
  | Colon -> ":"
  | Semicolon -> ";"
  | Minus -> "-"
  | Plus -> "+"
  | Asterisk -> "*"
  | Slash -> "/"
  | Tilde -> "~"
  | Exclamation -> "!"
  | Question -> "?"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "="
  | EqualEqual -> "=="
  | NotEqual -> "!="
  | LessThan -> "<"
  | LessThanOrEqual -> "<="
  | GreaterThan -> ">"
  | GreaterThanOrEqual -> ">="
  | Comma -> ","
  | ReturnKeyword -> "RETURN"
  | IntKeyword -> "INT"
  | IfKeyword -> "IF"
  | ElseKeyword -> "ELSE"
  | ForKeyword -> "FOR"
  | BreakKeyword -> "BREAK"
  | ContinueKeyword -> "CONTINUE"
  | Int i -> Printf.sprintf "INT<%d>" i
  | Id str -> Printf.sprintf "ID<%s>" str
