open Spaces
open Player

type board = { turn : int; spaces : spaces; players : player * player }

let new_board players = { turn = 0; spaces = new_spaces; players }

let token_can_move_to_space tok space { spaces; players; _ } =
  let player1, player2 = players in
  let p1t1, p1t2 = player1 in
  let p2t1, p2t2 = player2 in
  let other_toks = List.filter (fun t -> t <> tok) [ p1t1; p1t2; p2t1; p2t2 ] in
  let current_level = level_at tok spaces in
  let next_level = level_at space spaces in
  let space_occupied = List.mem space other_toks in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  in_adjacent && (not space_occupied) && next_level <> 4
  && next_level <= current_level + 1

let tokens_height tok { spaces; _ } = SpacesMap.find tok spaces

exception Bad_token

let move_token_to_space tok space board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1 in
  if tok = p1t1 then { board with players = ((space, p1t2), player2) }
  else if tok = p1t2 then { board with players = ((p1t1, space), player2) }
  else raise Bad_token

let token_can_build_on_space tok space board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1 in
  let p2t1, p2t2 = player2 in
  let all_toks = List.filter (fun t -> t <> tok) [ p1t1; p1t2; p2t1; p2t2 ] in
  let space_occupied = List.mem space all_toks in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  let space_level = level_at space board.spaces in
  let tok_on_win_space = tokens_height tok board = 3 in 
  in_adjacent && (not space_occupied) && space_level < 4 && (not tok_on_win_space)

let build_on_space space board =
  { board with spaces = build_on space board.spaces }

let complete_turn board =
  { board with turn = board.turn + 1; players = swap_players board.players }

let spaces_tok_can_move_to tok board : space list =
  List.filter
    (fun s -> token_can_move_to_space tok s board)
    (adjacent_spaces tok)

let spaces_tok_can_build_on tok board : space list =
  List.filter
    (fun s -> token_can_build_on_space tok s board)
    (adjacent_spaces tok)

let possible_moves_for_tok tok board =
  spaces_tok_can_move_to tok board
  |> List.map (fun s ->
         (tok, (s, spaces_tok_can_build_on s (move_token_to_space tok s board))))


let play_full_turn tok to_space build_space board =
  let after_move = move_token_to_space tok to_space board in
  if tokens_height to_space board = 3 then complete_turn after_move
  else build_on_space build_space after_move |> complete_turn

let heights_around_token tok board =
  List.map
    (fun s -> SpacesMap.find s board.spaces)
    (spaces_tok_can_move_to tok board)
