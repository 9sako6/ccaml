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

val inspect : program -> string
val op_of_string : unary_op -> string
