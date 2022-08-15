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

val inspect : program -> string
