(*2 Level 2 : Les Extractions*)

let grid_sample =
[[[[5; 3; 0]; [6; 0; 0]; [0; 9; 8]];
[[0; 7; 0]; [1; 9; 5]; [0; 0; 0]];
[[0; 0; 0]; [0; 0; 0]; [0; 6; 0]]];
[[[8; 0; 0]; [4; 0; 0]; [7; 0; 0]];
[[0; 6; 0]; [8; 0; 3]; [0; 2; 0]];
[[0; 0; 3]; [0; 0; 1]; [0; 0; 6]]];
[[[0; 6; 0]; [0; 0; 0]; [0; 0; 0]];
[[0; 0; 0]; [4; 1; 9]; [0; 8; 0]];
[[2; 8; 0]; [0; 0; 5]; [0; 7; 9]]]];;
(*
 let extract_square grille n =
  let rec count grille n x =
    match grille with
    |[]-> failwith " not good number"
    |d::l -> match d with 
      |e::d -> if n=x then flatten e
               else count (d::l) n (x+1)      
    |_-> count l n x 
       
  in count grille n 1;;
*)

  
(*2.1 Grille*)

let rec append liste1 liste2 =
  match liste1 with
    [] -> liste2
   | e::[] -> e :: liste2
   | e::l1 -> e :: append l1 liste2;;
  
let extract_square grille n =
  let rec count grille n x = match grille with
    |[] -> failwith "no match"
    |(e::l)::grille -> if n=x then flatten e
                       else count (l::grille) n (x+1)
    |e::l -> count l n x
  in
count grille n 1;;
(*extract_square grid_sample 5;;*)

(*2.2 Ligne*)
let rec prend l n = match l with
  |[] -> []
  |e::l -> if n=1 then e
	   else prend l (n-1);;
      
let extract_row grille n  =
  if n<1 || n >9 then failwith "no match"
  else let extract grille n = match n with
	 |n when n<4 ->         append (prend (prend (prend grille 1)   1)     n) (append (prend (prend (prend grille 1)  2)      n) (prend (prend (prend grille 1)   3)    n))
	 |n when n>3 && n<7 ->  append (prend (prend (prend grille 2) (1)) (n-3)) (append (prend (prend (prend grille 2) (2)) (n-3)) (prend (prend (prend grille 2) (3)) (n-3)))
	 |n when n>6 && n<10 -> append (prend (prend (prend grille 3) (1)) (n-6)) (append (prend (prend (prend grille 3) (2)) (n-6)) (prend (prend (prend grille 3) (3)) (n-6)))
	 |_ -> failwith "this value has no match"
       in
       extract grille n;;
(*extract_row grid_sample 5;;*)
