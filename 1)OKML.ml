(*Exercice 1*)

let a = let a = 3 and b = 2 in let a = a + b and b = a - b in a * a - b * b;;
let a =
  let a = 3 and b = 2 in
  let a = a + b and b = a - b in
  a*a - b*b;;

(*Exercice 2*)

int_of_float (1.5);;
let x=16 in sqrt (x);;

(*Exercice 3*)

'A';;
int_of_char ('A');;
char_of_int (65);;
'\065';;
'1';;
let espace = ' ';;
let x = "hello"and y = "world !";;
x^y;;
x^espace^y;;
let espace = "";;
x ^ espace ^ y;;
print_string ("hello world !");;
print_string ("hello world !\n");;
let cri = let x = "cou"in x ^ x;;
"";;
String.length (cri);;
String.length ("");;
cri.[0];;
cri.[5];;
cri.[6];;

let refrain = " on entend le " ^ cri in " Dans la foret profonde " ^ refrain ^ cri ^ "." ^ refrain ^ " " ^ cri;;

(*Exercice 4*)

let c = 1 and x = 5 in (c + 4) > (8 - x);;
10 = 10.0;;
1/2 = 0;;
let pair = let n = 13 in 2*(n/2) = n;;
2000 mod 4 = 0;;
"ab" = "ba";;
"durand" < "martin";;
"bonjour" > "bon";;
"une longue chaîne" > "une petite";;
'a' < 'Z';;
(4 < 8) && ("a" = "b");;
1/0 = 0;;
true || (1/0 = 0);;
false && (1/0 = 0);;
true <> false;;

(*Exercice 5*)

let a = 12.5;;
let x = 1;;
let y = x/2;;
let x = 1 in 2*x + x*x;;
let x = 1 in x <= 3;;
let x = 1 and y=2 in x<10 && y>0;;
let x = 1 in (0 < x) && x < 12;;
let a = 1 in a + 1;;

(*Exercice 6*)

1/2 = 0;;
1/0;;
1. /. 0.;;
0. /. 0.;;
let n = 5 in 2*n/2;;
let n = 5 in 2 * (n/2);;
123456 * 100000;;
max_int ;;
max_int + 1;;
min_int;;
exp(40.);;
exp(40.) = exp(40.) +. 10.;;

(*Exercice 7*)

(*Exercice 8*)

let annee = 2012 in
let fday = 0 in
let dif = annee - 2000
let b = (n/4)
let nb = dif - b
if(annee mod 4 = 0 && annee mod 100 <> 0) || (annee mod 400 = 0) = true
then ((n/4)*
"christmas day is on"
else
"christmas day is on"
