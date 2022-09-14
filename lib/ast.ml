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

type block_item =
  | Statement of statement
  | Declaration of declaration

and statement =
  | Return of exp
  | Exp of exp option
  (* exp is controlling condition.
     The first statement is 'if' branch.
     The second statement is 'else' branch. *)
  | If of exp * statement * statement option
  | For of exp option * exp * exp option * statement
  | ForDecl of declaration * exp * exp option * statement
  | Block of block_item list

(* declaration is not a statement. *)
and declaration =
  (* string is variable name, exp is optional initializer *)
  | Declare of string * exp option

type id = Id of string
type function_def = Function of (id * block_item list)
type program = Program of function_def

let rec inspect_exp indent =
  let next_indent = indent ^ " " in
  function
  | Var name -> Printf.sprintf "%s↳ Var(name: %s)\n" indent name
  | Assign (name, exp) ->
      Printf.sprintf "%s↳ Assign(name: %s)\n%s" indent name
        (inspect_exp (indent ^ " ") exp)
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
  | Condition (condition_exp, if_exp, else_exp) ->
      Printf.sprintf "%s%s?\n%s%s:\n%s"
        (inspect_exp next_indent condition_exp)
        next_indent
        (inspect_exp next_indent if_exp)
        next_indent
        (inspect_exp next_indent else_exp)

let rec inspect_statement indent statement =
  let next_indent = indent ^ " " in
  match statement with
  | Return exp ->
      Printf.sprintf "%s↳ Return\n%s" indent (inspect_exp next_indent exp)
  | Exp exp_option -> (
      match exp_option with
      | None -> Printf.sprintf "%s↳ Exp(null)" indent
      | Some exp ->
          Printf.sprintf "%s↳ Exp\n%s" indent (inspect_exp next_indent exp))
  | If (exp, statement_for_if, statement_for_else_option) -> (
      let exp_string = inspect_exp indent exp in
      let if_string =
        Printf.sprintf "%s↳ If\n%s%s" indent exp_string
          (inspect_statement next_indent statement_for_if)
      in
      match statement_for_else_option with
      | None -> if_string
      | Some statement ->
          if_string
          ^ Printf.sprintf "%s↳ Else\n%s" indent
              (inspect_statement next_indent statement))
  | For _ -> ""
  | ForDecl _ -> ""
  | Block block_items ->
      let block_items_string =
        Util.join (List.map (inspect_block_item (indent ^ " ")) block_items)
      in
      Printf.sprintf "%s↳ Block\n%s" indent block_items_string

and inspect_declaration indent = function
  | Declare (name, exp_option) -> (
      match exp_option with
      | Some exp ->
          let exp_string = inspect_exp (indent ^ " ") exp in
          Printf.sprintf "%s↳ Declare(name: %s)\n%s" indent name exp_string
      | None -> Printf.sprintf "%s↳ Declare(name: %s)\n" indent name)

and inspect_block_item indent = function
  | Statement statement -> inspect_statement indent statement
  | Declaration declaration -> inspect_declaration indent declaration

let inspect_function_def indent = function
  | Function (Id name, block_items) ->
      let block_items_string =
        Util.join (List.map (inspect_block_item (indent ^ " ")) block_items)
      in
      Printf.sprintf "%s↳ Function(name: %s)\n%s" indent name block_items_string

let inspect program_node =
  match program_node with
  | Program function_def_node ->
      let indent = " " in
      Printf.sprintf "Program\n%s"
        (inspect_function_def indent function_def_node)
