(*2 Chiffrement*)

(*2.1 Outils*)
let explode stri =
  let rec expl stri n = match n with
    |n when String.length stri = n -> []
    |n -> (stri.[n])::(expl stri (n+1))
  in 
expl  stri 0;;
(*explode "Chiffrement";;*)

let rec implode l = match l with
  |[] -> ""
  |e::l -> (Char.escaped e)^(implode l);;
(*implode ['C';'h';'i';'f';'f';'r';'e';'m';'e';'n';'t'];;*)

(*2.2 Chiffre de César*)

let s_rotn n x = match n with
  |n when ((int_of_char x)>65 && (int_of_char x)<78) || ((int_of_char x)>97 && (int_of_char x)<110) -> char_of_int((int_of_char x) - n)
  |n when char_of_int((int_of_char x)+n);;
s_rotn 13 'y';;

