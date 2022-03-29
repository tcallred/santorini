open Spaces
open Player

type serial_player = {
  card : string;
  tokens : space list option; [@default None]
}
[@@deriving yojson]

type ser_player_list = serial_player list [@@deriving yojson]

let card_to_string (card : card) : string =
  match card with
  | NoCard -> ""
  | Apollo -> "Apollo"
  | Artemis -> "Artemis"
  | Atlas -> "Atlas"
  | Demeter -> "Demeter"
  | Hephastus -> "Hephastus"
  | Minotaur -> "Minotaur"
  | Prometheus -> "Prometheus"
  | Pan -> "Pan"

let string_to_card (s : string) : card =
  match s with
  | "Apollo" -> Apollo
  | "Artemis" -> Artemis
  | "Atlas" -> Atlas
  | "Demeter" -> Demeter
  | "Hephastus" -> Hephastus
  | "Minotaur" -> Minotaur
  | "Prometheus" -> Prometheus
  | "Pan" -> Pan
  | _ -> NoCard

let tokens_to_list (t1, t2) : space list = [ t1; t2 ]

let list_to_tokens (tokens : token list) =
  let open List in
  (nth tokens 0, nth tokens 1)

let player_to_serial (player : player) : serial_player =
  {
    card = card_to_string player.card;
    tokens = Some (tokens_to_list player.tokens);
  }

let serial_to_player (s_player : serial_player) : player =
  {
    card = string_to_card s_player.card;
    tokens = list_to_tokens (Option.get s_player.tokens);
  }

let string_to_ser_player_list str =
  str |> Yojson.Safe.from_string |> ser_player_list_of_yojson

let ser_player_list_to_string lst =
  lst |> ser_player_list_to_yojson |> Yojson.Safe.to_string

let choose_start_position (lst : ser_player_list) : ser_player_list =
  let fst_player = List.nth lst 0 in
  let snd_player = List.nth lst 1 in
  [
    snd_player;
    {
      fst_player with
      tokens =
        (match snd_player.tokens with
        | None -> Some [ random_space (); random_space () ]
        | Some toks ->
            Some [ random_space_not_in toks; random_space_not_in toks ]);
    };
  ]
