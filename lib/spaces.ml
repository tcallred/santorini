open Utils

type space = int * int [@@deriving yojson]

module Space = struct
  type t = space

  let compare = compare
end

module SpacesMap = Map.Make (Space)

type spaces = int SpacesMap.t

let space_coords = list_comp (range_ab 1 6) (range_ab 1 6)

let space_coords2d =
  List.map (fun i -> List.map (fun j -> (i, j)) (range_ab 1 6)) (range_ab 1 6)

let new_spaces =
  List.fold_left
    (fun spaces space -> SpacesMap.add space 0 spaces)
    SpacesMap.empty space_coords

let list_of_spaces spaces =
  List.map
    (fun row -> List.map (fun space -> SpacesMap.find space spaces) row)
    space_coords2d

let spaces_of_list (lst : int list list) : spaces =
  let open List in
  fold_left
    (fun (map : spaces) (i : int) ->
      fold_left
        (fun (map : spaces) (j : int) ->
          SpacesMap.add (i + 1, j + 1) (nth (nth lst i) j) map)
        map (range 5))
    SpacesMap.empty (range 5)

let build_on space spaces =
  SpacesMap.update space
    (fun space_val ->
      Option.map
        (fun v ->
          assert (v < 4);
          v + 1)
        space_val)
    spaces

let top_off space spaces =
  SpacesMap.update space
    (fun space_val -> Option.map (fun _ -> 4) space_val)
    spaces

let level_at = SpacesMap.find
let can_build_on space spaces = level_at space spaces < 4

let adjacent_spaces (row, col) =
  [
    (row - 1, col - 1);
    (row - 1, col);
    (row - 1, col + 1);
    (row, col - 1);
    (row, col + 1);
    (row + 1, col - 1);
    (row + 1, col);
    (row + 1, col + 1);
  ]
  |> List.filter (fun (r, c) -> r >= 1 && r <= 5 && c >= 1 && c <= 5)

let rec random_space () : space =
  let s1, s2 = (Random.int 5 + 1, Random.int 5 + 1) in
  if s1 = s2 then random_space () else (s1, s2)

let rec random_space_not_in (lst : space list) : space =
  let s1, s2 = (Random.int 5 + 1, Random.int 5 + 1) in
  if List.mem (s1, s2) lst || s1 = s2 then random_space_not_in lst else (s1, s2)
