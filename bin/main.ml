let read file_name =
  let ch = open_in file_name in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write file_name text =
  let ch = open_out file_name in
  output_string ch text;
  close_out ch;
  ()

let file_name = Sys.argv.(1)

let () =
  read file_name |> Lexer.tokenize |> Parser.parse |> Assembly.transpile
  |> write "assembly.s"
