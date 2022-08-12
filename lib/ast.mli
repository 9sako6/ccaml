type exp = Const of int
type statement = Return of exp
type id = Id of string
type function_def = Function of (id * statement list)
type program = Program of function_def

val inspect : program -> string
