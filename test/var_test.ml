open Ccaml

let test_stack_index () =
  let context = Var.empty in
  let context = Var.declare "height" 8 context in
  let context = Var.declare "width" 8 context in
  Alcotest.(check int) "stack index" (-16) (Var.stack_index context)

let test_raise_redefinition () =
  let context = Var.empty in
  let variable_name = "name" in
  let context = Var.declare variable_name 8 context in
  let run () =
    try
      match Var.declare variable_name 8 context with
      | _ -> Alcotest.fail "No exception was thrown"
    with e -> raise e
  in
  Alcotest.check_raises "variable redefinition" Var.Redefinition run

let () =
  Alcotest.run "Var"
    [
      ( "stack_index",
        [
          Alcotest.test_case "with two 8 bytes variables" `Quick
            test_stack_index;
        ] );
      ( "declare",
        [
          Alcotest.test_case "variable redefinition" `Quick
            test_raise_redefinition;
        ] );
    ]
