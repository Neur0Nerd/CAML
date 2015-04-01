let rec length l = match l with
  |[] -> 0
  |[e] -> 1
  |e::l -> length l + 1;;
(*length [1;2;3;4];;*)

let rec flatten l = match l with
  |[] -> []
  |[]::l -> flatten l
  |(e1::e2)::l -> e1::(flatten e2::l)   ;;
(*flatten [[1;2;3];[4;5]];;*)

let rec check x l = match l with
  |[] -> false
  |e::[] when x=e -> true
  |e::l -> check x l;;
check 7 [1;2;3;4;5;6];;

let rec remove x l = match l with
  |[] -> []
  |e::[] when e<>x -> failwith"the value you are looking for is not present in the list"
  |e::l -> if x=e then l
    else e::(remove x l);;
(*remove 7 [1;2;3;4;5;6;7;7];;*)


let rec remove_all x l = match l with
  |[] -> []
  |e::[] -> l
  |e::l -> if x=e then remove_all x l
    else e::(remove_all x l);;
let rec list_uniq l = match l with 
  |[] -> []
  |e::b -> e::(list_uniq (remove_all e b));;
(*list_uniq [1;2;3;5;6;8;0;0;0;2;0;3;0;5;6;0;1;4];;*)

let rec list_match l1 l2 = match (l1,l2) with
  |([],l) | (l,[]) -> list_uniq l
  |(l1,l2) -> list_uniq (flatten (l1::l2::[]));;
(*list_match [1;2;3;5;6;8;0;0;0] [2;0;3;0;5;6;0;1;4];;*)


let rec grid_make_rectangle x y = match (x,y) with
  |(0,0) -> []
  |(0,y) -> []::(grid_make_rectangle x (y-1))
  |(x,y) -> 0::(grid_make_rectangle (x-1) (y));;
  
grid_make_rectangle 1 7;;
