module VarMap = Map.Make (String)

type var_map = (int * int) VarMap.t

let empty = VarMap.empty
let mem name = VarMap.mem name

let add name size map =
  let max_offset (map : var_map) =
    let keys_and_values = VarMap.bindings map in
    let rec max current_max = function
      | [] -> current_max
      | (_key, (offset, _size)) :: rest ->
          if offset > current_max then max offset rest else max current_max rest
    in
    max 0 keys_and_values
  in
  let offset = max_offset map in
  VarMap.add name (offset + size, size) map

let find name = VarMap.find name

let find_offset name map =
  match find name map with
  | offset, _size -> offset

let pp map =
  VarMap.iter
    (fun key value -> print_endline (Printf.sprintf "%s: %d" key value))
    map
