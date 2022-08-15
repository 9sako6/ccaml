type unary_op =
  | Negate (* - *)
  | Complement (* ~ *)
  | Not (* ! *)

type binary_op =
  | Add
  | Sub
  | Mult
  | Div
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

type exp =
  | Const of int
  | UnaryOp of unary_op * exp
  | BinaryOp of binary_op * exp * exp

type statement = Return of exp
type id = Id of string
type function_def = Function of (id * statement list)
type program = Program of function_def

let rec inspect_exp indent =
  let next_indent = indent ^ " " in
  function
  | Const n -> Printf.sprintf "%s↳ Const(value: %s)\n" indent (string_of_int n)
  | UnaryOp (operator, expression) ->
      let operator =
        match operator with
        | Negate -> "-"
        | Complement -> "~"
        | Not -> "!"
      in
      Printf.sprintf "%s↳ %s\n%s" indent operator
        (inspect_exp next_indent expression)
  | BinaryOp (operator, left_exp, right_exp) ->
      let operator =
        match operator with
        | Add -> "+"
        | Sub -> "-"
        | Mult -> "*"
        | Div -> "/"
        | And -> "&&"
        | Or -> "||"
        | Equal -> "=="
        | NotEqual -> "!="
        | LessThan -> "<"
        | LessThanOrEqual -> "<="
        | GreaterThan -> ">"
        | GreaterThanOrEqual -> ">="
      in
      Printf.sprintf "%s↳ %s\n%s%s" indent operator
        (inspect_exp next_indent left_exp)
        (inspect_exp next_indent right_exp)

let inspect_statement indent = function
  | Return e ->
      Printf.sprintf "%s↳ Return\n%s" indent (inspect_exp (indent ^ " ") e)

let inspect_function_def indent = function
  | Function (Id name, statements) ->
      let statements_string =
        Util.join (List.map (inspect_statement (indent ^ " ")) statements)
      in
      Printf.sprintf "%s↳ Function(name: %s)\n%s" indent name statements_string

let inspect program_node =
  match program_node with
  | Program function_def_node ->
      Printf.sprintf "Program\n%s" (inspect_function_def " " function_def_node)
