(*6 Level 4 : Encoder moi tout ça !*)

(*let string_to_char_list str = 
word_to_morse(
  let rec iter i liste =
    if i<0 then liste
    else iter (i-1) ((str.[i])::liste)
  in
    iter ((String.length str) - 1) [])
;;

let rec char_list_to_string_list str  = match str with
  |e::[] -> String.make 1 e 
  |e::l -> (String.make 1 e ) ^ (char_list_to_string_list l );;

let final str = 
char_list_to_string_list str
  let rec balo str = match str with
    |[] -> []
    |e::l -> e^(balo l) ;;*)
