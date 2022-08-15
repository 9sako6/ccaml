open Ccaml

let test_split () =
  Alcotest.(check (list string))
    "split hello"
    [ "h"; "e"; "l"; "l"; "o" ]
    (Util.split "hello")

let test_join () =
  Alcotest.(check string)
    "join string list" "hello"
    (Util.join [ "h"; "e"; "l"; "l"; "o" ])

let test_uniq_id expected_id () =
  Alcotest.(check int) "generate unique id" expected_id (Util.unique_id ())

(* Run tests *)
let () =
  Alcotest.run "Util"
    [
      ( "Util.split",
        [ Alcotest.test_case "split simple string" `Quick test_split ] );
      ( "Util.join",
        [ Alcotest.test_case "join simple string list" `Quick test_join ] );
      ( "Util.uniq_id",
        [
          Alcotest.test_case "1st call" `Quick (test_uniq_id 0);
          Alcotest.test_case "2nd call" `Quick (test_uniq_id 1);
        ] );
    ]
