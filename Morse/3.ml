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


let rec to_single_list l = match l with
  |[[]] -> []
  |e::(e1::l) when e1 <> [] -> e::(to_single_list (e1::l))
  |e::l -> to_single_list l
  |

to_single_list [['-'; '-'; '-']; ['-'; '.'; '-']];;
