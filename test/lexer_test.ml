let test_tokenize () =
  Alcotest.(check (list string))
    "return 29 program"
    [ "INT"; "ID<main>"; "("; ")"; "{"; "RETURN"; "INT<29>"; ";"; "}" ]
    (List.map Lexer.token_to_string
       (Lexer.tokenize "int main(){\n    return 29;\n}"))

(* Run tests *)
let () =
  Alcotest.run "Lexer"
    [
      ( "Lexer.tokenize",
        [ Alcotest.test_case "return 29 program" `Quick test_tokenize ] );
    ]
