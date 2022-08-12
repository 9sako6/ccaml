type unary_op =
  | Negate (* - *)
  | Complement (* ~ *)
  | Not (* ! *)

type exp =
  | Const of int
  | UnaryOp of unary_op * exp

type statement = Return of exp
type id = Id of string
type function_def = Function of (id * statement list)
type program = Program of function_def

let op_of_string = function
  | Negate -> "-"
  | Complement -> "~"
  | Not -> "!"

let rec inspect_exp indent = function
  | Const n -> Printf.sprintf "%s↳ Const(value: %s)\n" indent (string_of_int n)
  | UnaryOp (operator, expression) ->
      let next_indent = indent ^ " " in
      Printf.sprintf "%s↳ %s\n%s" indent (op_of_string operator)
        (inspect_exp next_indent expression)

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
