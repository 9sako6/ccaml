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
  | Condition of exp * exp * exp

type statement =
  | Return of exp
  | Exp of exp
  (* exp is controlling condition.
     The first statement is 'if' branch.
     The second statement is 'else' branch. *)
  | If of exp * statement * statement option

(* declaration is not a statement. *)
type declaration =
  (* string is variable name, exp is optional initializer *)
  | Declare of string * exp option

type block_item =
  | Statement of statement
  | Declaration of declaration

type id = Id of string
type function_def = Function of (id * block_item list)
type program = Program of function_def

val inspect : program -> string
