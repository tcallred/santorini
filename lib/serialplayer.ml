open Spaces
open Player

type serial_player = { card : string; tokens : space list } [@@deriving yojson]

let card_to_string (card : card) : string =
  match card with
  | NoCard -> ""
  | Apollo -> "Apollo"
  | Artemis -> "Artemis"
  | Atlas -> "Atlas"
  | Demeter -> "Demeter"
  | Hephastus -> "Hephastus"

let string_to_card (s : string) : card =
  match s with
  | "Apollo" -> Apollo
  | "Artemis" -> Artemis
  | "Atlas" -> Atlas
  | "Demeter" -> Demeter
  | "Hephastus" -> Hephastus
  | _ -> NoCard

let tokens_to_list (t1, t2) : space list = [ t1; t2 ]

let list_to_tokens (tokens : token list) =
  let open List in
  (nth tokens 0, nth tokens 1)

let player_to_serial (player : player) : serial_player =
  { card = card_to_string player.card; tokens = tokens_to_list player.tokens }

let serial_to_player (s_player : serial_player) : player =
  {
    card = string_to_card s_player.card;
    tokens = list_to_tokens s_player.tokens;
  }
