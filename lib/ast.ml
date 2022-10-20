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
  | FunCall of string * exp list (* string is function name *)
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
  | Break
  | Continue

(* declaration is not a statement. *)
and declaration =
  (* string is variable name, exp is optional initializer *)
  | Declare of string * exp option

type id = Id of string

type func =
  | Function of {
      name : string;
      params : string list;
      (*
        A function definition has some block items.
        A function declaration has none.
      *)
      body : block_item list option;
    }

type program = Program of func list

let rec inspect_exp indent =
  let next_indent = indent ^ " " in
  function
  | FunCall (name, _params) -> Printf.sprintf "%s↳ Call(name: %s)\n" indent name
  | Var name -> Printf.sprintf "%s↳ Var(name: %s)\n" indent name
  | Assign (name, exp) ->
      Printf.sprintf "%s↳ Assign(name: %s)\n%s" indent name
        (inspect_exp next_indent exp)
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
  | ForDecl (declaration, condition_exp, post_exp_option, statement) ->
      let init_string = inspect_declaration next_indent declaration in
      let condition_string = inspect_exp next_indent condition_exp in
      let post_string =
        match post_exp_option with
        | None -> ""
        | Some exp -> inspect_exp next_indent exp
      in
      let statement_string = inspect_statement next_indent statement in
      Printf.sprintf "%s↳ For\n%s%s%s%s" indent init_string condition_string
        post_string statement_string
  | For (init_exp_option, condition_exp, post_exp_option, statement) ->
      let init_string =
        match init_exp_option with
        | None -> ""
        | Some exp -> inspect_exp next_indent exp
      in
      let condition_string = inspect_exp next_indent condition_exp in
      let post_string =
        match post_exp_option with
        | None -> ""
        | Some exp -> inspect_exp next_indent exp
      in
      let statement_string = inspect_statement next_indent statement in
      Printf.sprintf "%s↳ For\n%s%s%s%s" indent init_string condition_string
        post_string statement_string
  | Block block_items ->
      let block_items_string =
        Util.join (List.map (inspect_block_item (indent ^ " ")) block_items)
      in
      Printf.sprintf "%s↳ Block\n%s" indent block_items_string
  | Break -> Printf.sprintf "%s↳ Break" indent
  | Continue -> Printf.sprintf "%s↳ Continue" indent

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

let inspect_function indent = function
  | Function { name; body = block_items_option; _ } ->
      let block_items_string =
        match block_items_option with
        | None -> ""
        | Some block_items ->
            Util.join (List.map (inspect_block_item (indent ^ " ")) block_items)
      in
      Printf.sprintf "%s↳ Function(name: %s)\n%s" indent name block_items_string

let inspect program_node =
  match program_node with
  | Program functions ->
      let indent = " " in
      let function_strings = List.map (inspect_function indent) functions in
      Printf.sprintf "Program\n%s" (Util.join function_strings)
