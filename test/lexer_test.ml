open Ccaml.Lexer

let test_tokenize program expected_tokens () =
  Alcotest.(check (list string))
    "test tokenize" expected_tokens
    (List.map Ccaml.Token.to_string (tokenize program))

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
        [
          Alcotest.test_case "return integer" `Quick
            (test_tokenize "int main(){\n    return 29;\n}"
               [
                 "INT"; "ID<main>"; "("; ")"; "{"; "RETURN"; "INT<29>"; ";"; "}";
               ]);
          Alcotest.test_case "assign var" `Quick
            (test_tokenize "int main(){int a = 2;a = 4;return a;}"
               [
                 "INT";
                 "ID<main>";
                 "(";
                 ")";
                 "{";
                 "INT";
                 "ID<a>";
                 "=";
                 "INT<2>";
                 ";";
                 "ID<a>";
                 "=";
                 "INT<4>";
                 ";";
                 "RETURN";
                 "ID<a>";
                 ";";
                 "}";
               ]);
        ] );
      ("inspect", [ Alcotest.test_case "return 29 program" `Quick test_inspect ]);
    ]
