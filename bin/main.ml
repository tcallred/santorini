open Santorini.Serialboard
open Santorini.Solver

let arg = Sys.argv.(1)

let next_board_state =
  arg |> string_to_board |> Result.get_ok |> board_after_chosen_move
  |> board_to_string

let () = Printf.printf "%s\n" next_board_state
