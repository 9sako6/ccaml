module VarMap = Map.Make (String)

type var = {
  index : int;
  size : int;
}

type var_map = var VarMap.t

type context = {
  stack_index : int;
  outer_scope : var_map;
  current_scope : var_map;
  break_label : string option;
  continue_label : string option;
}

let empty =
  {
    stack_index = 0;
    outer_scope = VarMap.empty;
    current_scope = VarMap.empty;
    break_label = None;
    continue_label = None;
  }

let make_new_scope context =
  let new_outer_scope =
    VarMap.union
      (fun _key _old_val new_val -> Some new_val)
      context.outer_scope context.current_scope
  in
  { context with outer_scope = new_outer_scope; current_scope = VarMap.empty }

let mem name = VarMap.mem name

exception Redefinition

let declare name size context =
  (* Check redefinition. *)
  let () = if mem name context.current_scope then raise Redefinition else () in
  let current_scope =
    VarMap.add name
      { index = context.stack_index - size; size }
      context.current_scope
  in
  { context with current_scope; stack_index = context.stack_index - size }

let add_param name size index context =
  let current_scope = VarMap.add name { index; size } context.current_scope in
  { context with current_scope }

let find name context =
  try VarMap.find name context.current_scope
  with Not_found -> VarMap.find name context.outer_scope

let set_berak_label label context = { context with break_label = Some label }

let set_continue_label label context =
  { context with continue_label = Some label }
