let rec split str =
  let str_length = String.length str in
  match str with
  | "" -> []
  | _ ->
      String.make 1 (String.get str 0)
      :: split (String.sub str 1 (str_length - 1))

let rec join = function
  | [] -> ""
  | head :: rest -> head ^ join rest
