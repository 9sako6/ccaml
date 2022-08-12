open Ccaml

let delimiter = "--"

(* Tests are executed in ./_build/default/test *)
let c_path file_name = Unix.realpath "../../../examples" ^ "/" ^ file_name

let _ =
  let rec run files =
    match files with
    | [] -> ()
    | head :: rest ->
        print_endline (delimiter ^ " " ^ head ^ " " ^ delimiter);
        c_path head |> Compiler.compile;
        run rest
  in
  run
    [
      "return_29.c";
      "return_exclamation_1.c";
      "return_minus_42.c";
      "return_tilde_42.c";
    ]
