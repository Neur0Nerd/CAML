(*6 Level 4 : Encoder moi tout ça !*)

(*let string_to_char_list str = 
word_to_morse(
  let rec iter i liste =
    if i<0 then liste
    else iter (i-1) ((str.[i])::liste)
  in
    iter ((String.length str) - 1) [])
;;

let final str = 
char_list_to_string_list str
  let rec balo str = match str with
    |[] -> []
    |e::l -> e^(balo l) ;;*)
*)

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
  |' ' -> ['/']		  
  | _  -> failwith "The type of your input is not a char, please try with another data";;  
  
let latin_to_morse str =
  let rec char_list_to_string_list str  = match str with
    |[] -> ""
    |e::l -> (String.make 1 e ) ^ (char_list_to_string_list l )
 in
let rec iter str i =
  if i < (String.length str) then (char_list_to_string_list(letter_to_morse(str.[i])))^(" " ^iter str (i+1))
  else ""
in
  iter str 0;;
(*latin_to_morse "Vive Les Listes" ;;*)
