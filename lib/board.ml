open Spaces
open Player

type board = { turn : int; spaces : spaces; players : player * player }

type move =
  | Move of { from : space; dest : space }
  | Build of space
  | Swap of { from : space; other : space }
  | TwiceMove of { from : space; first : space; second : space }
  | TwiceBuild of { first : space; second : space }
  | TopOff of space
  | DoubleBuild of space
  | Push of { from : space; victim : space; pushed_to : space }
  | BuildMove of { build : space; from : space; dest : space }

exception Bad_token
exception Bad_move

let new_board players = { turn = 0; spaces = new_spaces; players }

let token_can_move_to_space tok space { spaces; players; _ } =
  let player1, player2 = players in
  let p1t1, p1t2 = player1.tokens in
  let p2t1, p2t2 = player2.tokens in
  let other_toks = List.filter (fun t -> t <> tok) [ p1t1; p1t2; p2t1; p2t2 ] in
  let current_level = level_at tok spaces in
  let next_level = level_at space spaces in
  let space_occupied = List.mem space other_toks in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  in_adjacent && (not space_occupied) && next_level <> 4
  && next_level <= current_level + 1

let tokens_height tok { spaces; _ } = SpacesMap.find tok spaces

let move_token_to_space tok space board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1.tokens in
  if tok = p1t1 then
    { board with players = ({ player1 with tokens = (space, p1t2) }, player2) }
  else if tok = p1t2 then
    { board with players = ({ player1 with tokens = (p1t1, space) }, player2) }
  else raise Bad_token

let token_can_build_on_space tok space board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1.tokens in
  let p2t1, p2t2 = player2.tokens in
  let all_toks = List.filter (fun t -> t <> tok) [ p1t1; p1t2; p2t1; p2t2 ] in
  let space_occupied = List.mem space all_toks in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  let space_level = level_at space board.spaces in
  in_adjacent && (not space_occupied) && space_level < 4

let build_on_space space board =
  { board with spaces = build_on space board.spaces }

let token_can_swap_to_space tok space { spaces; players; _ } =
  let _, player2 = players in
  let p2t1, p2t2 = player2.tokens in
  let current_level = level_at tok spaces in
  let next_level = level_at space spaces in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  in_adjacent && next_level <> 4
  && next_level <= current_level + 1
  && (space = p2t1 || space = p2t2)

let token_can_push_into_space tok space { spaces; players; _ } =
  let _, player2 = players in
  let p2t1, p2t2 = player2.tokens in
  let push_space = opposite_side tok space in
  let can_go_into_push_space =
    on_board push_space spaces && level_at push_space spaces < 4
  in
  let current_level = level_at tok spaces in
  let next_level = level_at space spaces in
  let in_adjacent = List.mem space (adjacent_spaces tok) in
  in_adjacent && next_level <> 4
  && next_level <= current_level + 1
  && (space = p2t1 || space = p2t2)
  && can_go_into_push_space

let perform_swap (from : token) (other : token) board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1.tokens in
  let p2t1, p2t2 = player2.tokens in
  {
    board with
    players =
      ( {
          player1 with
          tokens =
            ( (if p1t1 = from then other else p1t1),
              if p1t2 = from then other else p1t2 );
        },
        {
          player2 with
          tokens =
            ( (if p2t1 = other then from else p2t1),
              if p2t2 = other then from else p2t2 );
        } );
  }

let perform_push from victim pushed_to board =
  let player1, player2 = board.players in
  let p1t1, p1t2 = player1.tokens in
  let p2t1, p2t2 = player2.tokens in
  {
    board with
    players =
      ( {
          player1 with
          tokens =
            ( (if p1t1 = from then victim else p1t1),
              if p1t2 = from then victim else p1t2 );
        },
        {
          player2 with
          tokens =
            ( (if p2t1 = victim then pushed_to else p2t1),
              if p2t2 = victim then pushed_to else p2t2 );
        } );
  }

let move_twice from first second board =
  move_token_to_space from first board |> move_token_to_space first second

let build_twice first second board =
  build_on_space first board |> build_on_space second

let top_off_space (space : space) board =
  { board with spaces = top_off space board.spaces }

let perform_double_build s board = build_on_space s board |> build_on_space s

let perform_build_move from build dest board =
  build_on_space build board |> move_token_to_space from dest

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

let spaces_tok_can_swap_with tok board : space list =
  List.filter
    (fun s -> token_can_swap_to_space tok s board)
    (adjacent_spaces tok)

let spaces_tok_can_push_into tok board : space list =
  List.filter
    (fun s -> token_can_push_into_space tok s board)
    (adjacent_spaces tok)

let play_move (m : move) board =
  match m with
  | Move { from; dest } -> move_token_to_space from dest board
  | Build s -> build_on_space s board
  | Swap { from; other } -> perform_swap from other board
  | TwiceMove { from; first; second } -> move_twice from first second board
  | TwiceBuild { first; second } -> build_twice first second board
  | TopOff s -> top_off_space s board
  | DoubleBuild s -> perform_double_build s board
  | Push { from; victim; pushed_to } -> perform_push from victim pushed_to board
  | BuildMove { from; build; dest } -> perform_build_move from build dest board

let possible_twice_moves (once_moves : move list) board : move list =
  List.map
    (fun m ->
      match m with
      | Move { from; dest } ->
          if tokens_height dest board = 3 then []
          else
            play_move (Move { from; dest }) board
            |> spaces_tok_can_move_to dest
            |> List.filter (fun s -> s <> from)
            |> List.map (fun s -> TwiceMove { from; first = dest; second = s })
      | _ -> raise Bad_move)
    once_moves
  |> List.concat

let possible_twice_builds (build_spaces : space list) : move list =
  Utils.list_comp build_spaces build_spaces
  |> List.filter (fun (s1, s2) -> s1 <> s2)
  |> List.map (fun (s1, s2) -> TwiceBuild { first = s1; second = s2 })

let possible_top_offs (build_spaces : space list) board : move list =
  build_spaces
  |> List.filter (fun s ->
         let h = tokens_height s board in
         0 <= h && h <= 2)
  |> List.map (fun s -> TopOff s)

let possible_double_builds (build_spaces : space list) board : move list =
  build_spaces
  |> List.filter (fun s ->
         let h = tokens_height s board in
         0 = h || h = 1)
  |> List.map (fun s -> DoubleBuild s)

let possible_build_moves tok board =
  spaces_tok_can_build_on tok board
  |> List.filter (fun s -> tokens_height s board < tokens_height tok board)
  |> List.map (fun s ->
         spaces_tok_can_move_to tok (play_move (Build s) board)
         |> List.map (fun ms -> BuildMove { from = tok; build = s; dest = ms }))
  |> List.concat

let wins_after_move m board card =
  match m with
  | Move { from; dest } -> (
      match card with
      | Pan ->
          tokens_height dest board = 3
          || tokens_height from board - tokens_height dest board >= 2
      | _ -> tokens_height dest board = 3)
  | TwiceMove { second; _ } -> tokens_height second board = 3
  | Push { victim; _ } -> tokens_height victim board = 3
  | BuildMove { dest; _ } -> tokens_height dest board = 3
  | _ -> false

let possible_action_seqs_for_tok tok board (card : card) : move list list =
  let move_actions =
    List.map
      (fun s -> Move { from = tok; dest = s })
      (spaces_tok_can_move_to tok board)
  in
  let first_actions =
    match card with
    | Apollo ->
        List.map
          (fun s -> Swap { from = tok; other = s })
          (spaces_tok_can_swap_with tok board)
        @ move_actions
    | Artemis -> possible_twice_moves move_actions board @ move_actions
    | Minotaur ->
        List.map
          (fun s ->
            Push { from = tok; victim = s; pushed_to = opposite_side tok s })
          (spaces_tok_can_push_into tok board)
        @ move_actions
    | Prometheus -> possible_build_moves tok board @ move_actions
    | _ -> move_actions
  in
  let action_seqs =
    List.map
      (fun m ->
        let board_after_first_action = play_move m board in
        let spot_after_first_action =
          match m with
          | Move { dest; _ } -> dest
          | Swap { other; _ } -> other
          | TwiceMove { second; _ } -> second
          | Push { victim; _ } -> victim
          | BuildMove { dest; _ } -> dest
          | _ -> raise Bad_move
        in
        let builds =
          spaces_tok_can_build_on spot_after_first_action
            board_after_first_action
        in
        let build_actions = List.map (fun b -> Build b) builds in
        let second_actions =
          match card with
          | Atlas -> possible_top_offs builds board @ build_actions
          | Demeter -> possible_twice_builds builds @ build_actions
          | Hephastus -> possible_double_builds builds board @ build_actions
          | _ -> build_actions
        in
        if wins_after_move m board card then [ [ m ] ]
        else List.map (fun second_act -> [ m; second_act ]) second_actions)
      first_actions
  in
  List.concat action_seqs

let play_full_turn (moves : move list) board =
  List.fold_left (fun b m -> play_move m b) board moves |> complete_turn

let heights_around_token tok board =
  List.map
    (fun s -> SpacesMap.find s board.spaces)
    (spaces_tok_can_move_to tok board)
