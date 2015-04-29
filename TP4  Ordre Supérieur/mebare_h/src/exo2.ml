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

let s_rotn n x =
  let i = int_of_char(x) in
  if i>64 && i<91 then
    let rec circle n i = match n with
      |0 -> char_of_int(i)
      |n when (i=90) && (n>0) -> circle (n-1) (65)
      |n -> circle (n-1) (i+1)
  in
  circle n i
  else
    let rec circle n i = match n with
      |0 -> char_of_int(i)
      |n when (i=122) && (n>0) -> circle (n-1) (97)
      |n -> circle (n-1) (i+1)
  in
  circle n i;;
(*s_rotn 13 'Y';;*)

let s_rot13 x = s_rotn 13 x;;
  (*s_rot13 'Y';;*)

let rot13 x =
  let exp = explode x in
  let rec rec_rot exp = match exp with
    |[] -> []
    |e::l -> (s_rot13 e)::(rec_rot l)
  in
  implode (rec_rot exp);;
(*rot13 "Chiffrement";;*)
