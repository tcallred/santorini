open Utils

module Space = struct
  type t = int * int

  let compare = compare
end

module SpacesMap = Map.Make (Space)

type t = int SpacesMap.t

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

let build_on space spaces =
  SpacesMap.update space
    (fun space_val ->
      Option.map
        (fun v ->
          assert (v < 4);
          v + 1)
        space_val)
    spaces

let level_at = SpacesMap.find
