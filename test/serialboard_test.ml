(* open Santorini.Serialboard
open Santorini.Spaces

let test_board_from_json () =
  let json_string =
    "{\"players\":[[[2,3],[4,4]],[[2,5],[3,5]]],\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],\"turn\":18}"
  in
  let (expected: Santorini.Board.board) =
    {
      turn = 18;
      spaces =
        [
          ((1, 1), 0);
          ((1, 2), 0);
          ((1, 3), 0);
          ((1, 4), 0);
          ((1, 5), 2);
          ((2, 1), 1);
          ((2, 2), 1);
          ((2, 3), 2);
          ((2, 4), 0);
          ((2, 5), 0);
          ((3, 1), 1);
          ((3, 2), 0);
          ((3, 3), 0);
          ((3, 4), 3);
          ((3, 5), 0);
          ((4, 1), 0);
          ((4, 2), 0);
          ((4, 3), 3);
          ((4, 4), 0);
          ((4, 5), 0);
          ((5, 1), 0);
          ((5, 2), 0);
          ((5, 3), 0);
          ((5, 4), 1);
          ((5, 5), 4);
        ]
        |> List.to_seq |> SpacesMap.of_seq;
      players = (((2, 3), (4, 4)), ((2, 5), (3, 5)));
    }
  in
  match string_to_board json_string with
  | Ok board' -> Alcotest.(check Santorini.Board.board) "same board" board' expected
  | Error s-> Alcotest.fail "Json string failed to parse: " ^ s *)
