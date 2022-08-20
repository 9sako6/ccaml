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
  | Var of string (* string is variable name *)
  | Assign of string * exp (* string is variable name *)
  | Const of int
  | UnaryOp of unary_op * exp
  | BinaryOp of binary_op * exp * exp

type statement =
  | Return of exp
  | Exp of exp
  | Declare of
      string
      * exp option (* string is variable name, exp is optional initializer *)

type id = Id of string
type function_def = Function of (id * statement list)
type program = Program of function_def

val inspect : program -> string
