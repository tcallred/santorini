let rec range_ab a b = if a >= b then [] else a :: range_ab (a + 1) b
let range = range_ab 0

let list_comp l1 l2 =
  let ( let* ) x f = List.map f x |> List.concat in
  let ( let+ ) x f = List.map f x in
  let* x = l1 in
  let+ y = l2 in
  (x, y)

let nth2d lst i j = List.nth (List.nth lst i) j
let nth3d lst i j k = List.nth (List.nth (List.nth lst k) j) i

let average lst =
  if List.length lst = 0 then 0
  else List.fold_left (fun a b -> a + b) 0 lst / List.length lst
