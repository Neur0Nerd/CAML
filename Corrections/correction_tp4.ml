(* random int list generator *)
let length l =
  let rec len i = function
    | [] -> i
    | e::l -> len (i+1) l
  in len 0 l;;

let gen_list n x =
  let rec gen l = function
    | 0 -> l
    | n -> gen (Random.int(x)::l) (n-1)
  in gen [] n;;

let liste = gen_list 20 10;;
gen_list 10 5;;
(* timer *)
let time f x = 
  let t = Sys.time() in
  let fx = f x in
  (Sys.time() -. t);;

let compare x y = (x <= y);;

let rec map f = function
    [] -> []
  |e::l -> (f e)::(map f l);;

let rec iter f = function
    [] -> ()
  |e::l -> f e; iter f l;;

let rec for_all f = function
    [] -> true
  |e::l when f e -> for_all f l
  | _ -> false;;

(* revert *)
let rev l = 
  let rec r a = function
    | [] -> a
    | e::l -> r (e::a) l
  in r [] l;;

(* bubble sort *)
let bubble_sort f l =
  let rec b o accu = function
    | [] -> (o,accu)
    | e::[] -> (o,e::accu)
    | e1::e2::l -> if (f e1 e2)
    then (b o (e1::accu) (e2::l))
    else (b true (e2::accu) (e1::l))
  and sort l = 
    let (o,l) = b false [] l in
    let l = rev l in
    if o then sort l else l
  in sort l;;

(* insertion sort *)
let insertion_sort f l = 
  let rec i_sort = function 
    | [] -> []
    | e::l -> insert e (i_sort l)
  and insert e = function
    | [] -> [e]
    | h::l -> if (f e h) 
    then e::h::l
    else h::(insert e l)
  in i_sort l;;

(* merge sort *)
let rec merge f = function
  | l, [] | [], l -> l
  | e1::l1,e2::l2 -> if (f e1 e2)
  then e1::(merge f (l1,e2::l2))
  else e2::(merge f (e1::l1,l2));;

let split l =
  let rec s (l1,l2) = function
    | [] -> (l1,l2)
    | e::l -> s (e::l2,l1) l
  in s ([],[]) l;;

let rec merge_sort f = function
  | [] -> []
  | e::[] -> [e]
  | l -> let (l1,l2) = split l in
    merge f (merge_sort f l1, merge_sort f l2);;

(* quick sort *)
let pivot f l =
  let rec p (l1,l2) = function
    | [] -> (l1,l2)
    | e::l -> if f e then p (l1,e::l2) l else p (e::l1,l2) l
  in p ([],[]) l;;

let rec quick_sort f = function
  | [] -> []
  | e::l -> let (left,right) = pivot (f e) l in
    (quick_sort f left) @ (e :: (quick_sort f right));;

time (bubble_sort compare) (gen_list 10000 10000);;

let time_compare n =
  let l = gen_list n n in
  print_string "timer for list with "; print_int n; print_string " elements"; print_newline();
  print_string "bubble sort: "; print_float (time (bubble_sort compare) l); print_newline();
  print_string "insertion sort: "; print_float (time (insertion_sort compare) l); print_newline();
  print_string "merge sort: "; print_float (time (merge_sort compare) l); print_newline();
  print_string "quick sort: "; print_float (time (quick_sort compare) l); print_newline();;
time_compare 10000;;

let s_rotn n c = 
  let x = int_of_char c in
  match x with
  | x when x >= int_of_char 'a' && x <= int_of_char 'z' -> char_of_int ((x - int_of_char 'a' + n) mod 26 + int_of_char 'a')
  | x when x >= int_of_char 'A' && x <= int_of_char 'Z' -> char_of_int ((x - int_of_char 'A' + n) mod 26 + int_of_char 'A')
  | _ -> char_of_int x;;

let s_rot13 = s_rotn 13;;

let explode s = 
  let rec exp i l =
    if i < 0 then l 
    else exp (i-1) (s.[i]::l) in
  exp (String.length s - 1) [];;

explode "Chiffrement";;
explode "test";;

map (rot13) (explode "test");;

let implode l =
  let rec imp res = function
    | [] -> res
    | e::l -> imp (res ^ (Char.escaped e)) l
  in imp "" l;;

let rot13 s = implode (map (s_rot13) (explode s));;

rot13 "Chiffrement";;
rot13 "Puvsserzrag";;

let encode l c = 
  let l = int_of_char l and c = int_of_char c and a = int_of_char 'A' in
  char_of_int ((l + c + 2 * a) mod 26 + a);;
let decode l c = 
  let l = int_of_char l and c = int_of_char c and a = int_of_char 'A' in
  char_of_int ((l - c + 2 * a) mod 26 + a);;

se_vigenere 'A' 'U';;

let vigenere f text clef =
  let text = explode text and clef = explode clef in
  let rec vig = function
  | [],_ -> []
  | t,[] -> vig (t,clef)
  | et::lt,lc when (int_of_char et < int_of_char 'A') or (int_of_char et > int_of_char 'Z') -> et :: vig (lt,lc)
  | et::lt,ec::lc -> (f et ec) :: vig (lt, lc)
  in implode (vig (text,clef));;

vigenere encode "ESPERONS QUE CET EXERCICE NE TOMBE PAS AU TP NOTE" "SECRETKEY";;
vigenere decode "V UVWHY IOIMBUL PM LSLYI XAOLM BU NAOJVUY" "SECRETKEY";;
