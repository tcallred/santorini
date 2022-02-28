(* open Yojson *)
open Spaces

type serial_board = {
  turn : int;
  spaces : int list list;
  players : space list list;
}
[@@deriving yojson]

let board_to_serial (board : Board.board) : serial_board =
  {
    turn = board.turn;
    spaces = Spaces.list_of_spaces board.spaces;
    players = Player.list_of_players board.players;
  }

let serial_to_board (s_board : serial_board) : Board.board =
  {
    turn = s_board.turn;
    spaces = Spaces.spaces_of_list s_board.spaces;
    players = Player.players_of_list s_board.players;
  }

let board_to_string (board : Board.board) =
  board |> board_to_serial |> serial_board_to_yojson |> Yojson.Safe.to_string

let string_to_board (str : string) =
  let open Ppx_deriving_yojson_runtime in
  str |> Yojson.Safe.from_string |> serial_board_of_yojson >|= fun x ->
  serial_to_board x

let json_string =
  "{\"players\":[[[2,3],[4,4]],[[2,5],[3,5]]],\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],\"turn\":18}"

let test_board = json_string |> string_to_board |> Result.get_ok