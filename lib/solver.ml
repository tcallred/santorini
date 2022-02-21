open Board
open Utils
open Option

let max_depth = 3

let score_tokens_standing tok board =
  match tokens_height tok board with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 4
  | 3 -> 20000
  | _ -> 0

let score_tokens_surroundings tok board =
  average (heights_around_token tok board)

let score_token tok board =
  score_tokens_standing tok board + (score_tokens_surroundings tok board / 3)

let win_condition board =
  let _, (t1, t2) = board.players in
  tokens_height t1 board = 3 || tokens_height t2 board = 3

let evaluate_position board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1 in
  let p2t1, p2t2 = player2 in
  let p1score = score_token p1t1 board + score_token p1t2 board in
  let p2score = score_token p2t1 board + score_token p2t2 board in
  p2score - p1score

let rec minimax (board : Board.board) maximizing_player alpha beta depth :
    Board.board option * int =
  if depth = max_depth || win_condition board then
    (some board, evaluate_position board)
  else
    let (t1, t2), _ = board.players in
    let moves =
      possible_moves_for_tok t1 board @ possible_moves_for_tok t2 board
    in
    let best = (none, if maximizing_player then Int.min_int else Int.max_int) in
    best_move moves board best alpha beta maximizing_player depth

and best_move moves board best alpha beta maximizing_player depth =
  match moves with
  | [] -> best
  | move :: rest_moves ->
      let _, curr_best_score = best in
      let tok, (to_space, build_moves) = move in
      let next_board, score =
        if tokens_height to_space board = 3 then
          let next = play_full_turn tok to_space (1, 1) board in
          minimax next (not maximizing_player) alpha beta (depth + 1)
        else
          best_build tok to_space build_moves board best alpha beta
            maximizing_player depth
      in
      if maximizing_player then
        let next_best =
          if score > curr_best_score then (next_board, score) else best
        in
        let _, next_best_score = next_best in
        let next_alpha = max alpha next_best_score in
        if beta <= alpha then next_best
        else
          best_move rest_moves board next_best next_alpha beta maximizing_player
            depth
      else
        let next_best =
          if score < curr_best_score then (next_board, score) else best
        in
        let _, next_best_score = next_best in
        let next_beta = max beta next_best_score in
        if beta <= alpha then next_best
        else
          best_move rest_moves board next_best alpha next_beta maximizing_player
            depth

and best_build tok to_space build_moves board best alpha beta maximizing_player
    depth =
  match build_moves with
  | [] -> best
  | build :: rest_builds ->
      let _, curr_best_score = best in
      let next_board = play_full_turn tok to_space build board in
      let _, score =
        minimax next_board (not maximizing_player) alpha beta (depth + 1)
      in
      if maximizing_player then
        let next_best =
          if score > curr_best_score then (some next_board, score) else best
        in
        let _, next_best_score = next_best in
        let next_alpha = max alpha next_best_score in
        if beta <= alpha then next_best
        else
          best_build tok to_space rest_builds board next_best next_alpha beta
            maximizing_player depth
      else
        let next_best =
          if score < curr_best_score then (some next_board, score) else best
        in
        let _, next_best_score = next_best in
        let next_beta = max beta next_best_score in
        if beta <= alpha then next_best
        else
          best_build tok to_space rest_builds board next_best alpha next_beta
            maximizing_player depth

let board_after_chosen_move (board : Board.board) : Board.board =
  let b, _ = minimax board true Int.min_int Int.max_int 0 in
  (* Printf.printf "Board with score %d" score; *)
  Option.get b
