let compile file_name =
  let read file_name =
    let ch = open_in file_name in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
    (* and write file_name text =
       let ch = open_out file_name in
       output_string ch text;
       close_out ch;
       () *)
  in
  let ast = read file_name |> Lexer.tokenize |> Parser.parse in
  let asm = Assembly.transpile ast in
  print_endline asm
