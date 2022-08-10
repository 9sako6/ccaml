let test_split () =
  Alcotest.(check (list string))
    "split hello"
    [ "h"; "e"; "l"; "l"; "o" ]
    (Util.split "hello")

let test_join () =
  Alcotest.(check string)
    "join string list" "hello"
    (Util.join [ "h"; "e"; "l"; "l"; "o" ])

(* Run tests *)
let () =
  Alcotest.run "Util"
    [
      ( "Util.split",
        [ Alcotest.test_case "split simple string" `Quick test_split ] );
      ( "Util.join",
        [ Alcotest.test_case "join simple string list" `Quick test_join ] );
    ]
