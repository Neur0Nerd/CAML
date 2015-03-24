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
