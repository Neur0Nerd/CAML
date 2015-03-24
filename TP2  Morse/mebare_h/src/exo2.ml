(* 4 Level 2 : Les mots *)

(*4.1*)
let rec word_to_morse l = match l with
  |[] -> []
  |e::l -> letter_to_morse e ::(word_to_morse l);;
(*word_to_morse ['S' ; 'O' ; 'S'];;*)

(*4.2*)
let rec to_single_list l = match l with
  |[] -> []
  |[e] -> e
  |e::l -> append (append e [' '] ) (to_single_list l);;
(*to_single_list [['-'; '-'; '-']; ['-'; '.'; '-']];;*)


let print word =  
    let rec impr l = match l with
      |[] -> []
      |e::l -> print_char e;
	       impr l
    in 
      impr  (to_single_list (word_to_morse word));;
(*print ['S' ; 'O' ; 'S'];;*)
