let rec range_ab a b = if a >= b then [] else a :: range_ab (a + 1) b
let range = range_ab 0

let list_comp l1 l2 =
  let ( let* ) x f = List.map f x |> List.concat in
  let ( let+ ) x f = List.map f x in
  let* x = l1 in
  let+ y = l2 in
  (x, y)
