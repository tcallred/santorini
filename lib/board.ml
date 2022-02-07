open Spaces
open Player
open Result

type board = { turn : int; spaces : spaces; players : player * player }

let new_board players = { turn = 0; spaces = new_spaces; players }

let token_can_move_to_space tok space { spaces; players; _ } =
  let player1, player2 = players in
  let p1t1, p1t2 = player1 in
  let p2t1, p2t2 = player2 in
  let other_toks = List.filter (fun t -> t = tok) [ p1t1; p1t2; p2t1; p2t2 ] in
  let current_level = level_at tok spaces in
  let next_level = level_at space spaces in
  let space_occupied = List.mem space other_toks in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  in_adjacent && (not space_occupied) && next_level <> 4
  && next_level <= current_level + 1

let move_token_to_space tok space board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1 in
  if tok = p1t1 then Ok { board with players = ((space, p1t2), player2) }
  else if tok = p1t2 then Ok { board with players = ((p1t1, space), player2) }
  else Error "token not found in player 1's tokens"

let token_can_build_on_space tok space { spaces; players; _ } =
  let player1, player2 = players in
  let p1t1, p1t2 = player1 in
  let p2t1, p2t2 = player2 in
  let all_toks = List.filter (fun t -> t = tok) [ p1t1; p1t2; p2t1; p2t2 ] in
  let space_occupied = List.mem space all_toks in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  let space_level = level_at space spaces in
  in_adjacent && (not space_occupied) && space_level < 4

let build_on_space space board =
  { board with spaces = build_on space board.spaces }
