(*1 Level 1 : Les Matrices ?*)

(*1.1 Rectangle*)
let rec zerolist x = match x with
    |0 -> []
    |x -> 0::(zerolist (x-1));;
let rec grid_make_rectangle x y = match y with
  |0-> []
  |y -> (zerolist x)::(grid_make_rectangle x (y-1));;
(*grid_make_rectangle 2 7;;*)

(*1.2 Carré*)
let grid_make_square x = grid_make_rectangle  x x;;
grid_make_square 7;;

(*let rec append liste1 liste2 =
  match liste1 with
    [] -> liste2
   | e::[] -> e :: liste2
   | e::l1 -> e :: append l1 liste2;;

 let grid a = match (int_of_float (sqrt(float_of_int a))) with
  |0 -> [[]]
  |a -> grid_make_square a;;
grid 9;;
let zerolist x  = 
  let rec zero x accu = match accu with
    |x -> []
    |_ -> (zero x (accu-1))::(grid x)
  in
zero x 0;;

zerolist 9;;*)

(*1.3 Grille*)
let grid_make x =
  let rec l1 x n = match n with
                     |0 -> []
                     |n -> (grid_make_square x)::(l1 x (n-1))
  in
    let rec l2 x n = match n with
      |0 -> []
      |n -> (l1 x x):: l2 x (n-1)
    in
      let x = int_of_float (sqrt (float_of_int x)) in l2 x x;;
