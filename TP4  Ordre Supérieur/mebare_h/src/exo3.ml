(*3 Les tris*)
  
(*3.1 Tri à bulles*)

(*let rec test f l = match l with
  |[] | _::[] -> true 
  |e1::e2::l when f e1 e2 -> test f (e2::l)
  |e1::e2::l -> false;;

let rec tri f l = match l with
  |e1::e2::l when (f e1 e2) -> e1::e2::(tri f l)
  |e1::e2::l -> e2::e1::(tri f l)
  |[] -> [];;


let bubble_sort f l =
  let test = iter f l in 
  if (test = l) then l
  else bubble_sort f test;;*)

(*3.2 Tri par insertion*)
let insertion_sort f l =
  let rec insert f x l = match l with
    |[] -> [x]
    |e::l when f x e -> x::e::l 
    |e::l -> e::(insert f x l) in
  let rec iter main l = match l with
    |[] -> main
    |e::l -> iter (insert f e main) l
  in iter [] l;;

  (*insertion_sort (<) [8;1;2;3;9;10;4;6;5;7];;*)

let rec tour f l = match l with
  |e1::e2::l when (f e1 e2) -> e1::(tour f (e2::l))
  | e1::e2::l -> e2::(tour f (e1::l))
  |l -> l;;

let rec bubble_sort f l =
  if tour f l = l then l
  else bubble_sort f l;;
