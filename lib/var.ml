module VarMap = Map.Make (String)

type var = {
  offset : int;
  size : int;
}

type var_map = var VarMap.t

type context = {
  outer_scope : var_map;
  current_scope : var_map;
  break_label : string option;
  continue_label : string option;
}

let empty =
  {
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
  {
    outer_scope = new_outer_scope;
    current_scope = VarMap.empty;
    break_label = context.break_label;
    continue_label = context.continue_label;
  }

let mem name = VarMap.mem name

let stack_index context =
  let outer_keys_and_values = VarMap.bindings context.outer_scope in
  let current_keys_and_values = VarMap.bindings context.current_scope in
  let rec max current_max = function
    | [] -> current_max
    | (_key, { offset; size = _ }) :: rest ->
        if offset > current_max then max offset rest else max current_max rest
  in
  let outer_stack_index = -max 0 outer_keys_and_values in
  let current_stack_index = -max 0 current_keys_and_values in
  outer_stack_index + current_stack_index

exception Redefinition

let declare name size context =
  (* Check redefinition. *)
  let () = if mem name context.current_scope then raise Redefinition else () in
  let offset = -stack_index context in
  let current_scope =
    VarMap.add name { offset = offset + size; size } context.current_scope
  in
  {
    outer_scope = context.outer_scope;
    current_scope;
    break_label = context.break_label;
    continue_label = context.continue_label;
  }

let find name context =
  try VarMap.find name context.current_scope
  with Not_found -> VarMap.find name context.outer_scope

let set_berak_label label context =
  {
    outer_scope = context.outer_scope;
    current_scope = context.current_scope;
    break_label = Some label;
    continue_label = context.continue_label;
  }

let set_continue_label label context =
  {
    outer_scope = context.outer_scope;
    current_scope = context.current_scope;
    break_label = context.break_label;
    continue_label = Some label;
  }
