(* 4 Level 2 : Les mots *)

(*4.1*)

let letter_to_morse = function
  |'a' | 'A' -> ['.' ; '-']
  |'b' | 'B' -> ['-' ; '.' ; '.' ; '.']
  |'c' | 'C' -> ['-' ; '.' ; '-' ; '.']
  |'d' | 'D' -> ['-' ; '.' ; '.']
  |'e' | 'E' -> ['.']
  |'f' | 'F' -> ['.' ; '.' ; '-' ; '.'] 
  |'g' | 'G' -> ['-' ; '-' ; '.']
  |'h' | 'H' -> ['.' ; '.' ; '.' ; '.']
  |'i' | 'I' -> ['.'; '.']
  |'j' | 'J' -> ['.' ; '-' ; '-' ; '-']
  |'k' | 'K' -> ['-' ; '.' ; '-']
  |'l' | 'L' -> ['.' ; '-' ; '.' ; '.']
  |'m' | 'M' -> ['-' ; '-']
  |'n' | 'N' -> ['-' ; '.']
  |'o' | 'O' -> ['-' ; '-' ; '-']
  |'p' | 'P' -> ['.' ; '-' ; '-' ; '.']
  |'q' | 'Q' -> ['-' ; '-' ; '.' ; '-']
  |'r' | 'R' -> ['.' ; '-' ; '.']
  |'s' | 'S' -> ['.' ; '.' ; '.']
  |'t' | 'T' -> ['-']
  |'u' | 'U' -> ['.' ; '.' ; '-']
  |'v' | 'V' -> ['.' ; '.' ; '.' ; '-']
  |'w' | 'W' -> ['.' ; '-' ; '-']
  |'x' | 'X' -> ['-' ; '.' ; '.' ; '-']
  |'y' | 'Y' -> ['-' ; '.' ; '-' ; '-']
  |'z' | 'Z' -> ['-' ; '-' ; '.' ; '.']
  | _  -> failwith "The type of your input is not a char, please try with another data";;

let rec word_to_morse l = match l with
  |[] -> []
  |e::l -> letter_to_morse e ::(word_to_morse l);;
(*word_to_morse ['S' ; 'O' ; 'S'];;*)

let rec append l1 l2 = match (l1,l2) with
  |([],[]) -> []
  |([],l2) -> l2
  |(l1,[]) -> l1
  |(e1::l1,l2) -> e1::(append l1 l2);;


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


(*5 Level 3 : Les phrases !*)


(*5.1 Conversion (encore)*)
let rec sentence_to_morse l = match l with
  |[] -> []
  |e::l -> (word_to_morse e):: (sentence_to_morse l);;
(*sentence_to_morse [['t'; 'o']; ['m'; 'e']];;*)

(*5.2 char list list . . .*)
let rec sentence_to_single_list l = match l with
  |[] -> []
  |e::l -> append (to_single_list e) ('/'::(sentence_to_single_list l));;
(*sentence_to_single_list [[['-']; ['-'; '-'; '-']]; [['-'; '-']; ['.']]];;*)

(*5.3 Sans escales*)
let rec to_single_morse l = match l with 
  |[] -> []
  |l -> to_single_list (word_to_morse l);;
(*to_single_morse ['S'; 'O'; 'S'] ;;*)

let rec latin_sentence_to_single l = match l with
  |[] -> []
  |l -> sentence_to_single_list (sentence_to_morse l);;
(*latin_sentence_to_single [['T'; 'O']; ['M'; 'E']] ;;*)

