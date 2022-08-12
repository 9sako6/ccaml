let test_transpile () =
  Alcotest.(check string)
    "return 29 program" "  .globl  main\nmain:\n  movl $29, %eax\n  ret\n"
    ("int main(){\n    return 29;\n}" |> Lexer.tokenize |> Parser.parse
   |> Assembly.transpile)

(* Run tests *)
let () =
  Alcotest.run "Assembly"
    [
      ( "transpile",
        [ Alcotest.test_case "return 29 program" `Quick test_transpile ] );
    ]
