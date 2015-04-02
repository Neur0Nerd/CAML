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
let extract_square grille n =
  let rec count grille n x = match grille with
    |[] -> failwith "no match"
    |(e::liste)::grille -> if n=x then flatten e
                       else count (liste::grille) n (x+1)
    |e::liste -> count liste n x
  in
count grille n 1;;
(*extract_square grid_sample 9;;*)
