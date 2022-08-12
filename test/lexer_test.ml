open Ccaml.Lexer

let test_tokenize () =
  Alcotest.(check (list string))
    "return 29 program"
    [ "INT"; "ID<main>"; "("; ")"; "{"; "RETURN"; "INT<29>"; ";"; "}" ]
    (List.map token_to_string (tokenize "int main(){\n    return 29;\n}"))

let test_inspect () =
  Alcotest.(check string)
    "return 29 program"
    "\"INT\", \"ID<main>\", \"(\", \")\", \"{\", \"RETURN\", \"INT<29>\", \
     \";\", \"}\""
    (inspect
       [
         IntKeyword;
         Id "main";
         OpenParen;
         CloseParen;
         OpenBrace;
         ReturnKeyword;
         Int 29;
         Semicolon;
         CloseBrace;
       ])

(* Run tests *)
let () =
  Alcotest.run "Lexer"
    [
      ( "tokenize",
        [ Alcotest.test_case "return 29 program" `Quick test_tokenize ] );
      ("inspect", [ Alcotest.test_case "return 29 program" `Quick test_inspect ]);
    ]
