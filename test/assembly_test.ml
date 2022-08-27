open Ccaml

let test_transpile () =
  Alcotest.(check string)
    "return 29 program"
    ".globl  main\n\
     main:\n\
    \  push %rbp\n\
    \  movq %rsp, %rbp\n\
    \  mov $29, %rax\n\
    \  mov %rbp, %rsp\n\
    \  pop %rbp\n\
    \  ret\n"
    ("int main(){\n    return 29;\n}" |> Lexer.tokenize |> Parser.parse
   |> Assembly.transpile)

let test_transpile_error invalid_program expected_error () =
  let run () =
    try
      match
        invalid_program |> Lexer.tokenize |> Parser.parse |> Assembly.transpile
      with
      | _ -> Alcotest.fail "No exception was thrown"
    with e -> raise e
  in
  Alcotest.check_raises "test_transpile_error" expected_error run

(* Run tests *)
let () =
  Alcotest.run "Assembly"
    [
      ( "transpile",
        [ Alcotest.test_case "return 29 program" `Quick test_transpile ] );
      ( "exception",
        [
          Alcotest.test_case "redefinition" `Quick
            (test_transpile_error
               (Util.read "../../../examples/invalid/redefvar.c")
               (Failure "redefinition of 'a'."));
        ] );
    ]
