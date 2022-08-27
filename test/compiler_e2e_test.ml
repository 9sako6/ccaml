open Ccaml

let delimiter = "--"

(* Tests are executed in ./_build/default/test *)
let c_path file_name = Unix.realpath "../../../examples/valid" ^ "/" ^ file_name

let valid_c_files =
  Sys.readdir "../../../examples/valid"
  |> Array.to_list
  |> List.sort (fun file1 file2 ->
         if file1 == file2 then 0 else if file1 < file2 then -1 else 1)

(* Check generated assembly. *)
let _ =
  let rec run files =
    match files with
    | [] -> ()
    | file_name :: rest ->
        print_endline (delimiter ^ " " ^ file_name ^ " " ^ delimiter);
        c_path file_name |> Compiler.compile;
        run rest
  in
  run valid_c_files

(* Check exit code. *)
let _ =
  let rec run files =
    match files with
    | [] -> ()
    | file_name :: rest -> (
        print_endline (delimiter ^ " " ^ file_name ^ " " ^ delimiter);
        Util.read (c_path file_name)
        |> Lexer.tokenize |> Parser.parse |> Assembly.transpile
        |> Util.write "asm.s";
        match Unix.system "gcc asm.s && ./a.out" with
        | WEXITED code ->
            print_endline (string_of_int code);
            run rest
        | _ -> failwith "Unexpected exit code.")
  in
  run valid_c_files
