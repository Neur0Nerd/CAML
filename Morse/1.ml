(*2 LEVEL 0 : Les Must-do*)

(*2.1 Egalite*)

let rec are_equal l1 l2 = match (l1,l2) with
  |([],[]) -> true
  |([],_)|(_,[]) -> false
  |(e1::x,e2::y) when e1=e2 -> are_equal x y
  |_ -> false;;

(*are_equal [1; 2; 3; 4] [1; 2; 3; 7; 7];;*)


(*2.2 Concatenation*)

let rec append l1 l2 = match (l1,l2) with
  |([],[]) -> []
  |([],l2) -> l2
  |(l1,[]) -> l1
  |(e1::l1,l2) -> e1::(append l1 l2);;

 (*append [1; 2; 3; 4] [1; 2; 3; 7; 7];;
 append [] [1];;*)

(*2.3 Inversion*)

let reverse l1 = 
  let rec rec_reverse l1 l2 = match l1 with
  |[] -> l2
  |e::l -> rec_reverse l (e::l2)
  in rec_reverse l1 [];;

(*reverse [1;2;4;5];;*)


(*2.4 Impression*)

let rec print_ l = match l with
  |[] -> ()
  |e::l1 -> print_char e;
            print_ l1;; 
(*print_ ['R'; 'O'; 'B'; 'I'; 'N'; ' '; 'C'; 'H'; 'W'; 'A'; 'N'];;*)
