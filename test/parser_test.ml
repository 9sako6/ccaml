open Ccaml

let test_parse_error invalid_program expected_error () =
  let run () =
    try
      match invalid_program |> Lexer.tokenize |> Parser.parse with
      | _ -> Alcotest.fail "No exception was thrown"
    with e -> raise e
  in
  Alcotest.check_raises "test_parse_error" expected_error run

(* Run tests *)
let () =
  Alcotest.run "Parser"
    [
      ( "parse error",
        [
          Alcotest.test_case "empty return program" `Quick
            (test_parse_error
               (Util.read "../../../examples/invalid/empty_return.c")
               (Failure "Parse error. `return` returns empty."));
          Alcotest.test_case "else without if" `Quick
            (test_parse_error
               (Util.read "../../../examples/invalid/else.c")
               (Failure "'else' without a previous 'if'."));
          Alcotest.test_case "using if as an expression" `Quick
            (test_parse_error
               (Util.read "../../../examples/invalid/if_assign.c")
               (Failure "expected expression before 'if'."));
          Alcotest.test_case "declaration in 'if'" `Quick
            (test_parse_error
               (Util.read "../../../examples/invalid/if_declaration.c")
               (Failure "expected expression before 'int'."));
          Alcotest.test_case "using 'else' twice" `Quick
            (test_parse_error
               (Util.read "../../../examples/invalid/if_else_else.c")
               (Failure "'else' without a previous 'if'."));
          Alcotest.test_case "'for' with too few clauses" `Quick
            (test_parse_error
               (Util.read
                  "../../../examples/invalid/syntax_err_too_few_for_clauses.c")
               (Failure "expected `;`."));
          Alcotest.test_case "extra `}`" `Quick
            (test_parse_error
               (Util.read "../../../examples/invalid/syntax_err_extra_brace.c")
               (Failure "too many `}`."));
        ] );
    ]
