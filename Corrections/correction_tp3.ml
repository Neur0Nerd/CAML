(* constantes *)
let grid_sample = 
  [[[[5; 3; 0]; [6; 0; 0]; [0; 9; 8]];
    [[0; 7; 0]; [1; 9; 5]; [0; 0; 0]];
    [[0; 0; 0]; [0; 0; 0]; [0; 6; 0]]];
   [[[8; 0; 0]; [4; 0; 0]; [7; 0; 0]];
    [[0; 6; 0]; [8; 0; 3]; [0; 2; 0]];
    [[0; 0; 3]; [0; 0; 1]; [0; 0; 6]]];
   [[[0; 6; 0]; [0; 0; 0]; [0; 0; 0]];
    [[0; 0; 0]; [4; 1; 9]; [0; 8; 0]];
    [[2; 8; 0]; [0; 0; 5]; [0; 7; 9]]]];;

(* prof only *)
let l1 = [1;2;3;4;5;6;7;8;9]
and l2 = [1;3;5;0;4;6;0;0;2]
and l3 = [0;1;3;4;0;0;0;1;9];;
let values = [1; 2; 3; 4; 5; 6; 7; 8; 9];;
let null = 0;;

(* longueur d'une liste en terminale *)
let length l = 
  let rec len r = function
    | [] -> r
    | e::l -> len (r+1) l
  in len 0 l;;

(* genere y listes de x listes de valeurs null *)
let grid_make_rectangle x y null = 
  let rec row null = function
    | 0 -> []
    | n -> null :: row null (n-1)
  in
  row (row null x) y;;

(* genere x listes de x listes de valeurs null *) 
let grid_make_square x null = 
  grid_make_rectangle x x null;;

(* racine carree entiere du nombre d'elements - sans verification - besoin d'un carre parfait *)
let dimensions elements = int_of_float (sqrt (float_of_int (length elements)));;

(* genere une matrice de matrice de dimension de taille racine carree de la liste d'elements *)
let grid_make elements null =
  let s = dimensions elements in
  grid_make_square s (grid_make_square s null);;

(* constante *)
let grid = grid_make values null;;

(* applatie une liste de liste en liste *)
let rec flatten = function 
  | [] -> []
  | e::l -> e @ (flatten l);;

flatten [[1;2;3];[4;5]];;
(* prof only - constante de test - prof only - box number , row number, column number, value *)
let grid_test =
  [[[[(1, 1, 1, 0);(1, 2, 1, 0);(1, 3, 1, 0)];[(1, 1, 2, 0);(1, 2, 2, 0);(1, 3, 2, 0)];[(1, 1, 3, 0);(1, 2, 3, 0);(1, 3, 3, 0)]];
    [[(2, 4, 1, 0);(2, 5, 1, 0);(2, 6, 1, 0)];[(2, 4, 2, 0);(2, 5, 2, 0);(2, 6, 2, 0)];[(2, 4, 3, 0);(2, 5, 3, 0);(2, 6, 3, 0)]];
    [[(3, 7, 1, 0);(3, 8, 1, 0);(3, 9, 1, 0)];[(3, 7, 2, 0);(3, 8, 2, 0);(3, 9, 2, 0)];[(3, 7, 3, 0);(3, 8, 3, 0);(3, 9, 3, 0)]]];
   [[[(4, 1, 4, 0);(4, 2, 4, 0);(4, 3, 4, 0)];[(4, 1, 5, 0);(4, 2, 5, 0);(4, 3, 5, 0)];[(4, 1, 6, 0);(4, 2, 6, 0);(4, 3, 6, 0)]];
    [[(5, 4, 4, 0);(5, 5, 4, 0);(5, 6, 4, 0)];[(5, 4, 5, 0);(5, 5, 5, 0);(5, 6, 5, 0)];[(5, 4, 6, 0);(5, 5, 6, 0);(5, 6, 6, 0)]];
    [[(6, 7, 4, 0);(6, 8, 4, 0);(6, 9, 4, 0)];[(6, 7, 5, 0);(6, 8, 5, 0);(6, 9, 5, 0)];[(6, 7, 6, 0);(6, 8, 6, 0);(6, 9, 6, 0)]]];
   [[[(7, 1, 7, 0);(7, 2, 7, 0);(7, 3, 7, 0)];[(7, 1, 8, 0);(7, 2, 8, 0);(7, 3, 8, 0)];[(7, 1, 9, 0);(7, 2, 9, 0);(7, 3, 9, 0)]];
    [[(8, 4, 7, 0);(8, 5, 7, 0);(8, 6, 7, 0)];[(8, 4, 8, 0);(8, 5, 8, 0);(8, 6, 8, 0)];[(8, 4, 9, 0);(8, 5, 9, 0);(8, 6, 9, 0)]];
    [[(9, 7, 7, 0);(9, 8, 7, 0);(9, 9, 7, 0)];[(9, 7, 8, 0);(9, 8, 8, 0);(9, 9, 8, 0)];[(9, 7, 9, 0);(9, 8, 9, 0);(9, 9, 9, 0)]]]];;

(*
 ** extrait une sous-matrice de notre matrice principale
 ** sous-matrices numerote de gauche a droite du haut vers le bas
 ** applatie en une liste de valeurs
 *)
let extract_square grid x elements = 
  let s = dimensions elements in
  let rec extract = function
    | _, [] -> failwith "out of bound"
    | 0, e::_ -> e
    | n, _::l -> extract (n-1, l)
  in flatten (extract ((x-1) mod s, (extract ((x-1) / s, grid))));;

extract_square grid_test 9 values;;
extract_square grid_sample 9 values;;

(* extrait la xeme ligne de notre matrice *)
let extract_row grid x elements =
  let s = dimensions elements 
  in
  let rec extract_1 = function
    | _, [] -> failwith "out of bound"
    | 0, e::_ -> e
    | n, _::l -> extract_1 (n-1, l)
  and extract_2 = function
	| _, [] -> []
	| n, e::l -> extract_1 (n, e) :: extract_2 (n, l)
  in flatten (extract_2 ((x-1) mod s, (extract_1 ((x-1) / s, grid))));; 

extract_row grid_test 1 values;;
extract_row grid_sample 5 values;;

(* extrait la ieme colonne de notre matrice *)
let extract_column grid x elements =
  let s = dimensions elements in
  let rec extract_1 = function
    | _, [] -> []
    | n, e::l -> extract_2 (n, e) :: extract_1 (n, l)
  and extract_2 = function
	| _, [] -> []
	| n, e::_ when n < s -> extract_3 (n, e)
	| n, _::l -> extract_2 (n-s, l)
  and extract_3 = function
    | _, [] -> []
    | n, e::l -> extract_4 (n, e) :: extract_3 (n, l)
  and extract_4 = function
    | _, [] -> failwith "out of bound"
    | 0, e::_ -> e
    | n, _::l -> extract_4 (n-1, l)
  in flatten (extract_1 (x-1,grid))
;;

extract_column grid_test 9 values;;
extract_column grid_sample 6 values;;

(* retourne la valeur de la case x y *)
let grid_value grid x y elements = 
  let row = extract_row grid y elements in
  let rec column = function
    | _, [] -> failwith "out of bound"
    | 0, e::_ -> e
    | n, _::l -> column (n-1,l)
  in column (x-1, row);;
grid_value grid_sample 8 9 values;;

(* affichage de la grille *)
let grid_print grid elements = 
    let rec print_row = function
      | [] -> print_newline()
      | e::l -> print_string ((string_of_int e) ^ " "); print_row l	    
    and print = function
      | 0 -> print_newline()
      | n -> print(n-1); print_row (extract_row grid n elements)
    in print_newline(); print (length elements);;
grid_print grid_sample values;;

(* prof only *)
let grid_print grid display elements = 
    let rec print_row = function
      | [] -> print_newline()
      | e::l -> print_string ((display e) ^ " "); print_row l	    
    and print = function
      | 0 -> print_newline()
      | n -> print(n-1); print_row (extract_row grid n elements)
    in print_newline(); print (length elements);;
grid_print grid_sample (string_of_int) values;;

(* 
 ** prof only
 ** sert uniquement a faire une gestion plus generic du bazar - bonus
 ** leurs faire utiliser l'operateur = sans le passer en parametre
 ** ils ne connaissent pas l'ordre superieur encore
 *)
let equal e1 e2 = 
  let (_,_,_,e) = e2
  in e = e1 ;;

(* verifie la presence d'un element x dans une liste par la fonction de comparaison *)
let rec check x = function
  | [] -> false 
  | e::l when e = x -> true
  | _::l -> check x l;;

(* prof only *)
let rec check f x = function
  | [] -> false 
  | e::l when f e x -> true
  | _::l -> check f x l;;

(* retire un element dans une liste *)
let rec remove x = function
  | [] -> []
  | e::l when e = x -> l
  | e::l -> e::(remove x l);;

(* prof only *)
let rec remove f x = function
  | [] -> []
  | e::l when f e x -> l
  | e::l -> e::(remove f x l);;

(* 
 ** permet de verifier que les elements sont dans une liste d'element autorises
 ** une seule et unique occurence est autorisee
 *)
let list_validate elements null liste =
  let rec validate elts = function
    | [] -> true
    | e::l when null = e -> validate elts l
    | e::l when not (check e elts) -> false
    | e::l -> validate (remove e elts) l
  in validate elements liste;;

(* prof only *)
let list_validate f elements null liste =
  let rec validate elts = function
    | [] -> true
    | e::l when f null e -> validate elts l
    | e::l when not (check f e elts) -> false
    | e::l -> validate (remove f e elts) l
  in validate elements liste;;

list_validate (equal) (values) (null) (extract_column grid_test 9 values);;
list_validate (=) values 0 (extract_column grid 9 values);;
list_validate (=) values 0 l1;;
list_validate (=) values 0 l2;;
list_validate (=) values 0 l3;;

(* valide qu'une grille ne contient pas d'element en double *)
let grid_validate grid elements null =
    let rec validate = function
      | 0 -> true
      | n -> 
	  list_validate elements null (extract_row grid n values) &&
	  list_validate elements null (extract_column grid n values) &&
	  list_validate elements null (extract_square grid n values) &&
	  validate (n-1)
    in validate (length elements);;

(* prof only *)
let grid_validate f grid elements null =
  let rec validate = function
    | 0 -> true
    | n -> 
	list_validate f elements null (extract_row grid n values) &&
	list_validate f elements null (extract_column grid n values) &&
	list_validate f elements null (extract_square grid n values) &&
	validate (n-1)
  in validate (length elements);;

(* verife qu'une grille ne contient plus de case libre *)
let grid_isfull grid null =
  let l = flatten (flatten (flatten grid)) in
  let rec isfull = function
    | [] -> true
    | e::_ when null = e -> false
    | _::l -> isfull l
  in isfull l;;

(* prof only *)
let grid_isfull f grid null =
  let l = flatten (flatten (flatten grid)) in
  let rec isfull = function
    | [] -> true
    | e::_ when f null e -> false
    | _::l -> isfull l
  in isfull l;;

grid_isfull (equal) grid_test null;;

(* retourne une liste des elements uniques de l1 *)
let list_uniq l1 = 
  let rec l_exist x = function
    | [] -> false
    | e::l when e = x -> true
    | _::l -> l_exist x l
  and l_match = function
    | [] -> []
    | e::l when not (l_exist e l) -> e::(l_match l)
    | _::l -> l_match l
  in l_match l1;;

(* prof only *)
let list_uniq f l1 = 
  let rec l_exist x = function
    | [] -> false
    | e::l when f e x -> true
    | _::l -> l_exist x l
  and l_match = function
    | [] -> []
    | e::l when not (l_exist e l) -> e::(l_match l)
    | _::l -> l_match l
  in l_match l1;;

list_uniq (=) [1;2;3;5;6;8;0;0;0;2;0;3;0;5;6;0;1;4];;

(* retourne la liste des elements uniques de l1 et l2 *)
let list_match l1 l2 =
  list_uniq (flatten (l1::[l2]));;
(* prof only *)
let e_equal e1 e2 = let (_,_,_,e1) = e1 and (_,_,_,e2) = e2 in e1 = e2;;
let list_match f l1 l2 = 
  list_uniq f (flatten (l1::[l2]));;

list_match (=) [1;2;3;5;6;8;0;0;0] [2;0;3;0;5;6;0;1;4];;


(* retourne la liste des elements de l2 non present dans l1 *)
let find_missing l1 l2 =
  let rec l_exist x = function
    | [] -> false 
    | e::l when e = x -> true
    | _::l -> l_exist x l
  and missing = function
    | [] -> []
    | e::l when not (l_exist e l1) -> e::(missing l)
    | _::l -> missing l
  in missing l2;;

(* prof only *)
let find_missing f l1 l2 =
  let rec l_exist x = function
    | [] -> false 
    | e::l when f e x -> true
    | _::l -> l_exist x l
  and missing = function
    | [] -> []
    | e::l when not (l_exist e l1) -> e::(missing l)
    | _::l -> missing l
  in missing l2;;

(list_uniq (=) (list_match (=) [1;2;3;5;6;8;0;0;0] [2;0;3;0;5;6;0;1;4]));;
find_missing (=) (list_uniq (=) (list_match (=) [1;2;3;5;6;8;0;0;0] [2;0;3;0;5;6;0;1;4])) values;;

(* retourne la liste des possibilites de valeurs pour une case *) 
let grid_find grid x y elements = 
  let s = dimensions elements in
  let case = (1 + (x-1)/s + ((y-1)/s)*s) in
  let row = extract_row grid y elements
  and column = extract_column grid x elements
  and mat = extract_square grid case elements 
  in find_missing (list_match (list_match row column) mat) values;;

(* prof only *)
let rec print_list = function
  | [] -> print_newline()
  | e::l -> print_int e; print_string " "; print_list l;;

let print_line label e liste = 
  print_string (label ^ "[");
  print_int e;
  print_string "]: ";
  print_list liste;;

let grid_find grid f x y elements = 
  let s = dimensions elements in
  let case = (1 + (x-1)/s + ((y-1)/s)*s) in
  let row = extract_row grid y elements
  and column = extract_column grid x elements
  and mat = extract_square grid case elements 
  in 
(*  print_newline();print_line "row" y row; print_line "col" x column; print_line "mat" case mat; *)
  find_missing f (list_match f (list_match f row column) mat) elements;;

grid_find grid_sample (=) 9 1 values;;
grid_find grid_sample (=) 1 9 values;;
grid_find grid_sample (=) 1 7 values;;

(*
 ** retourne une liste de lignes de solutions pour une case
 ** la reponse ne contient pas la valeur de la case si celle si est differente de null
 *)
let grid_full_find grid elements = 
  let l = length elements in 
  let rec iter_row = function
    | n when n > l -> []
    | n -> (iter_column n 1)::(iter_row (n+1)) 
  and iter_column x = function
    | y when y > l -> []
    | y -> (grid_find grid y x elements)::(iter_column x (y+1))
  in iter_row 1;;

(* prof only *)
let grid_full_find grid f elements = 
  let l = length elements in 
  let rec iter_row = function
    | n when n > l -> []
    | n -> (iter_column n 1)::(iter_row (n+1)) 
  and iter_column x = function
    | y when y > l -> []
    | y -> (grid_find grid f y x elements)::(iter_column x (y+1))
  in iter_row 1;;

grid_full_find grid_sample (=) values;;

(* solution rang n+1 *)

let print x y e = print_string ("[" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ "]: " ^ (string_of_int e) ^ "\n");; 
let grid_nsolve grid f elements null =
  let s = dimensions elements in
  let rec iter_1 = function
    | n when n >= s -> []
    | n -> (iter_2 n 0) :: (iter_1 (n+1))
  and iter_2 y1 = function
    | n when n >= s -> []
    | n -> (iter_3 y1 n 1) :: (iter_2 y1 (n+1))
  and iter_3 y1 x1 = function
    | n when n > s -> []
    | n -> (iter_4 y1 x1 n 1) :: (iter_3 y1 x1 (n+1))
  and iter_4 y1 x1 y2 = function
    | n when n > s -> []
    | n -> (solve x1 y1 n y2) :: (iter_4 y1 x1 y2 (n+1))
  and solve x1 y1 x2 y2 = 
    let x = x1 * s + x2 and y = y1 * s + y2 in
    let case = grid_value grid x y elements in
    if (f case null) then
      let solution = grid_find grid f x y values in
      match solution with 
      | [] -> case
      | e::[] -> e
      | e::l -> case
    else
      case
  in iter_1 0;;

grid_print grid_sample (string_of_int) values;;
grid_print (grid_nsolve grid_sample (=) values null) (string_of_int) values;;

(* solution d'une grille *)
let solve grid elements null =
  let rec check grid =
    if (grid_isfull grid null) then grid
    else check (grid_nsolve grid elements null)
  in check grid;;

(* prof only *)
let solve grid f elements null =
  let rec check grid =
    if (grid_isfull f grid null) then grid
    else check (grid_nsolve grid f elements null)
  in check grid;;

let grid_solved = solve grid_sample (=) values null;;
grid_validate (=) grid_solved values null;;
grid_print grid_solved (string_of_int) values;;

