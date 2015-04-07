(*1 Les obligatoires*)

(*1.1 Générateur de liste*)
let rec gen_list l max = match l with
  |l when l=0 -> []
  |l -> (Random.int max)::(gen_list (l-1) max);;
(*gen_list 10 5;;*)

(*1.2 Fonction map*)
let map f l = 
  let rec map2 f l l2 = match l with
  |[] -> []
  |e::l -> (f e)::(map2 f l l2)
  in
map2 f l [];;
(*map (function x -> x * x) [1; 2; 3; 4];;*)

(*1.3 Fonction iter*)
let rec iter f l = match l with
  |[] -> ()
  |e::l -> (f e);
           iter f l;;
(*iter (print_int) [1; 2; 3; 4];;*)

(*1.4 Fonction for_all*)
let rec for_all f l = match l with
  |[] -> true
  |e::l -> (f e) && for_all f l;;
(*for_all (function x -> x = 0) [0; 0; 0; 0];;*)
