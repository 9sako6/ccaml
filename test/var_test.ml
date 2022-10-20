open Ccaml

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
      ( "declare",
        [
          Alcotest.test_case "variable redefinition" `Quick
            test_raise_redefinition;
        ] );
    ]
