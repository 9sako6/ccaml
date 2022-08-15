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

let counter = ref 0

let unique_id () =
  let id = !counter in
  incr counter;
  id

let write file_name text =
  let ch = open_out file_name in
  output_string ch text;
  close_out ch;
  ()

let read file_name =
  let ch = open_in file_name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
