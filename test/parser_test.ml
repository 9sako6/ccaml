open Ccaml

let test_parse_error () =
  let invalid_program = "int main() {\n    return;\n}" in
  let f () =
    try
      match invalid_program |> Lexer.tokenize |> Parser.parse with
      | _ -> Alcotest.fail "No exception was thrown"
    with e -> raise e
  in
  Alcotest.check_raises "" (Failure "Parse error. `return` returns empty.") f

(* Run tests *)
let () =
  Alcotest.run "Parser"
    [
      ( "exception",
        [ Alcotest.test_case "empty return program" `Quick test_parse_error ] );
    ]
