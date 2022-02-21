open Spaces
open Utils

type token = space
type player = token * token
type player_list = space list list [@@deriving yojson]

let list_of_players ((p1t1, p1t2), (p2t1, p2t2)) =
  [ [ p1t1; p1t2 ]; [ p2t1; p2t2 ] ]

let players_of_list (lst : token list list) : player * player =
  ((nth2d lst 0 0, nth2d lst 0 1), (nth2d lst 1 0, nth2d lst 1 1))

let swap_players (player1, player2) : player * player = (player2, player1)

let string_to_player_list (str : string) =
  str |> Yojson.Safe.from_string |> player_list_of_yojson

let player_list_to_string (lst : player_list) =
  lst |> player_list_to_yojson |> Yojson.Safe.to_string

let choose_start_position (lst : player_list) : player_list =
  if List.length lst = 0 then [ [ random_space (); random_space () ] ]
  else
    let player2 =
      [
        [
          random_space_not_in (List.nth lst 0);
          random_space_not_in (List.nth lst 0);
        ];
      ]
    in
    List.append lst player2
