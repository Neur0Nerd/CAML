(*Exercice 1.1*)

(*1*)
let moyenne x y = (x+y)/2;;
(*2*)
let moyenne2 (x,y) = (x+y)/2;;

#trace moyenne;;
#trace moyenne2;;

(*Exercice 1.2*)

(*1*)

let rec fibo n =
  if n<0 then failwith "negative vaule won't work with fibo function"
  else
    match n with
    |0 | 1 -> 0
    |n -> fibo(n-1)+ fibo(n-2);;
#trace fibo;;

let rec acker c = match c with 
  |(0,n) -> n+1
  |(m,0) -> acker ((m-1), 1)
  |(m,n) -> acker ((m-1), acker (m,(n-1)));;
#trace acker;;

(*2*)

let rec pow p = match p with
  |(x,0) -> 1
  |(x,n) -> x*(pow(x,(n-1)));;
#trace pow;;

let rec pow2 p = match p with
  |(x,0) -> 1
  |(x,n) when n mod 2 = 0 -> pow2(x*x,n/2)
  |(x,n) -> x*pow2((x*x),(n/2));;
#trace pow2;;
