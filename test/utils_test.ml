open Santorini.Utils

let test_range () =
  Alcotest.(check (list int)) "same lists" [0;1;2] (range 3)

let () =
  let open Alcotest in
  run "Utils" [
    "range", [
      test_case "Range works" `Quick test_range
    ]
  ]