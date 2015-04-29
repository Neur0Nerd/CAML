(* data *)
let morse_letters = 
  [('A', ".-");   ('B', "-..."); ('C', "-.-."); ('D', "-..");  ('E', ".");
   ('F', "..-."); ('G', "--.");  ('H', "...."); ('I', "..");   ('J', ".---");
   ('K', "-.-");  ('L', ".-.."); ('M', "--");   ('N', "-.");   ('O', "---");
   ('P', ".--."); ('Q', "--.-"); ('R', ".-.");  ('S', "...");  ('T', "-");
   ('U', "..-");  ('V', "...-"); ('W', ".--");  ('X', "-..-"); ('Y', "-.--");
   ('Z', "--..")] ;;

let morse_numbers =
  [('1', ".----"); ('2', "..---"); ('3', "...--");
   ('4', "....-"); ('5', "....."); ('6', "-....");
   ('7', "--..."); ('8', "---.."); ('9', "----.");
   ('0', "-----")] ;;

let morse = morse_letters @ morse_numbers;;

(* level 0 *)
let rec are_equal a b = match (a,b) with
    ([],[]) -> true
  | (e::l,e2::l2) when e = e2 -> are_equal l l2
  | _ -> false;;
are_equal [1, 2, 3] [1, 2, 3];;
are_equal ['a', 'b', 'c'] ['a', 'b', 'c', 'd'];;

let rec append l1 l2 = match l1 with
  | []   -> l2
  | e::l -> e::(append l l2) ;;
append [1; 2; 3] [4; 5] ;;

let rec reverse = function
    [] -> []
  | e::l -> let li = reverse l in concatene li [e];;
let reverse_term list =
  let rec rev_rec list accu = match list with
      [] -> accu
    | e::l -> rev_rec l (e::accu)
  in rev_rec list [];;
reverse ['a'; 'l'; 'l'; 'o'];;

let rec print_list = function
  | [] -> ()
  | e::l -> print_char e ; print_list l ;;
print_list ['-';'-';'-';' ';'.';'.';'.';' ';'-';'-';'-';' '] ;;

(* requierements *)

let rec assoc x = function
  |  [] -> failwith "assoc:not found"
  | (a,b)::l when a = x -> b
  | _::l -> assoc x l ;;
assoc 2 [(1,'a'); (2,'b'); (3,'c')] ;;
assoc 4 [(1,'a'); (2,'b'); (3,'c')] ;;

let list_of_string s = 
  let long = String.length s in
  let rec construit i =
    if i = long then
      []
    else
      s.[i] :: construit (i+1)
  in
  construit 0 ;;
list_of_string "--.-" ;;
list_of_string "SOS" ;;
        
(* level 1 *)
let rec is_morse = function
  | [] -> true
  | e::l -> (e = '-' || e = '.') && is_morse l;;
is_morse ['-'; '.'; '-'; '3'; 'a'];;

let letter_to_morse c = 
  list_of_string (assoc (Char.uppercase c) morse) ;;
letter_to_morse '3' ;;

(* level 2 *)
let rec word_to_morse = function
  | [] -> []
  | e::l -> (letter_to_morse e)::(word_to_morse l);;
word_to_morse (list_of_string "SOS") ;;

let rec to_single_list = function
  | [] -> []
  | e::[] -> e
  | e::l -> append e (' '::(to_single_list l)) ;;

to_single_list [['.'; '.'; '.']; ['-'; '-'; '-']; ['.'; '.'; '.']] ;;

let rec print_morse = function
  | [] -> ()
  | e::l -> print_list e; print_char ' '; print_morse l;;
let print_morse_2 list = print_list (to_single_list list) ;;
print_morse [['.'; '.'; '.']; ['-'; '-'; '-']; ['.'; '.'; '.']] ;;
print_morse_2 [['.'; '.'; '.']; ['-'; '-'; '-']; ['.'; '.'; '.']] ;;

(* level 3 *)
let rec sentence_to_morse = function
  | [] -> []
  | e::l -> (word_to_morse e)::sentence_to_morse l;;
sentence_to_morse [['T'; 'O']; ['M'; 'E']] ;;

let rec sentence_to_single_list = function
  | [] -> []
  | e::l -> let li = to_single_list e in
    append li ('/'::sentence_to_single_list l) ;;
sentence_to_single_list [[['-']; ['-'; '-'; '-']]; [['-'; '-']; ['.']]];;

let to_single_morse list =
  let rec to_single_morse_rec = function
    | [] -> []
    | e::l -> (letter_to_morse e)::(to_single_morse_rec l)
  in to_single_list (to_single_morse_rec list);;
let to_single_morse list = to_single_list (word_to_morse list) ;;
to_single_morse ['S'; 'O'; 'S'] ;;

let latin_sentence_to_single list =
  let rec too_much_list_latin_rec = function
    | [] -> []
    | e::l -> (word_to_morse e)::(too_much_list_latin_rec l)
  in sentence_to_single_list (too_much_list_latin_rec list);;
let latin_sentence_to_single list = sentence_to_single_list (sentence_to_morse list) ;;
latin_sentence_to_single [['T'; 'O']; ['M'; 'E']] ;;

(* level 4 *)
let rec string_of_list = function
  | [] -> ""
  | e::l -> Char.escaped (Char.uppercase e)  ^ string_of_list l ;;
let latin_to_morse_word s = 
  let word = list_of_string s in
 string_of_list (to_single_morse word) ;;
latin_to_morse_word "ACDC" ;;

let is_not_letter c = 
  let n = int_of_char c in 
  not ((n >= 65 && n <= 90) || (n >= 97 && n <= 122) || (n >= 48 && n <= 57))

let list_of_word s = 
  let list_of_sub i j = list_of_string (String.sub s i (j-i)) and long = String.length s in
  let rec build i j =
    if j = long then
      list_of_sub i j::[]
    else
      if is_not_letter(s.[j]) then
	list_of_sub i j:: build (j+1) (j+1)
      else
	build i (j+1)
  in
  build 0 0 ;;
list_of_word "TO ME" ;;

let latin_to_morse s = string_of_list (latin_sentence_to_single (list_of_word s)) ;;
latin_to_morse "Vive Les Listes" ;;

(* level 5+ *)
(* ils sont autonomes sur ces parties : ie google *)
