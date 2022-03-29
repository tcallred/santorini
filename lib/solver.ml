open Board
open Utils
open Option

let max_depth = 3
let win_score = 20000

let score_tokens_standing tok board =
  match tokens_height tok board with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 4
  | 3 -> win_score
  | _ -> 0

let score_tokens_surroundings tok board =
  average (heights_around_token tok board)

let score_token tok board =
  score_tokens_standing tok board + (score_tokens_surroundings tok board / 3)

let player_win_condition (player : Player.player) (prev_player : Player.player)
    board prev_board =
  match player.card with
  | Pan ->
      let t1, t2 = player.tokens in
      let pt1, pt2 = prev_player.tokens in
      tokens_height t1 board = 3
      || tokens_height t2 board = 3
      || tokens_height pt1 prev_board - tokens_height t1 board >= 2
      || tokens_height pt2 prev_board - tokens_height t2 board >= 2
  | _ ->
      let t1, t2 = player.tokens in
      tokens_height t1 board = 3 || tokens_height t2 board = 3

let win_condition board prev_board =
  let player1, player2 = board.players in
  let prev_player2, prev_player1 = prev_board.players in
  player_win_condition player1 prev_player1 board prev_board
  || player_win_condition player2 prev_player2 board prev_board

let evaluate_position board prev_board =
  let player1, player2 = board.players in
  let prev_player2, prev_player1 = prev_board.players in
  let p1t1, p1t2 = player1.tokens in
  let p2t1, p2t2 = player2.tokens in
  let p1score =
    if player_win_condition player1 prev_player1 board prev_board then win_score
    else score_token p1t1 board + score_token p1t2 board
  in
  let p2score =
    if player_win_condition player2 prev_player2 board prev_board then win_score
    else score_token p2t1 board + score_token p2t2 board
  in
  p2score - p1score

let rec minimax (board : Board.board) (prev_board : Board.board)
    maximizing_player alpha beta depth : Board.board option * int =
  if depth = max_depth || win_condition board prev_board then
    (some board, evaluate_position board prev_board)
  else
    let player1, _ = board.players in
    let t1, t2 = player1.tokens in
    let moves =
      possible_action_seqs_for_tok t1 board player1.card
      @ possible_action_seqs_for_tok t2 board player1.card
      |> List.sort (fun a b -> List.length a - List.length b)
    in
    let best = (none, if maximizing_player then Int.min_int else Int.max_int) in
    best_move moves board best alpha beta maximizing_player depth

and best_move move_seqs board best alpha beta maximizing_player depth =
  match move_seqs with
  | [] -> best
  | move_seq :: rest_moves ->
      let _, curr_best_score = best in
      let next_board = play_full_turn move_seq board in
      let _, score =
        minimax next_board board (not maximizing_player) alpha beta (depth + 1)
      in
      if maximizing_player then
        let next_best =
          if score > curr_best_score then (some next_board, score) else best
        in
        let _, next_best_score = next_best in
        let next_alpha = max alpha next_best_score in
        if beta <= next_alpha then
          next_best
        else
          best_move rest_moves board next_best next_alpha beta maximizing_player
            depth
      else
        let next_best =
          if score < curr_best_score then (some next_board, score) else best
        in
        let _, next_best_score = next_best in
        let next_beta = min beta next_best_score in
        if next_beta <= alpha then
          next_best
        else
          best_move rest_moves board next_best alpha next_beta maximizing_player
            depth

let board_after_chosen_move (board : Board.board) : Board.board =
  (* let _ = print_endline "Entering" in *)
  let b, _ = minimax board board true Int.min_int Int.max_int 0 in
  (* Printf.printf "Board with score %d" score; *)
  Option.get b
