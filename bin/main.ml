open Santorini.Serialboard
open Santorini.Solver
open Santorini.Player

let rec main () =
  let arg = read_line () in
  if arg = "" then exit 0
  else
    let next_board_state =
      match string_to_board arg with
      | Ok board -> board_after_chosen_move board |> board_to_string
      | Error _ ->
          string_to_player_list arg |> Result.get_ok |> choose_start_position
          |> player_list_to_string
    in

    Printf.printf "%s\n" next_board_state;
    main ()
;;

main ()
