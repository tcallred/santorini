open Spaces
open Utils

type token = space
type player = token * token

let list_of_players ((p1t1, p1t2), (p2t1, p2t2)) =
  [ [ p1t1; p1t2 ]; [ p2t1; p2t2 ] ]

let players_of_list (lst : token list list) : player * player =
  ((nth2d lst 0 0, nth2d lst 0 1), (nth2d lst 1 0, nth2d lst 1 1))
