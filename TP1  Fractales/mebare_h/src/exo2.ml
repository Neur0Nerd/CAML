(*2 FRACTALES*)

#load "graphics.cma" ;;
open Graphics;;
open_graph "";;
open Random;;
clear_graph();;

(*2.1*)

let draw x y l h =
  clear_graph();
  moveto x (y + h/2);
  rlineto (-l/2) (-h);
  rlineto (l) (2*h/3);
  rlineto (-l) 0;
  rlineto (l) (-2*h/3);
  rlineto (-l/2) (h);;

draw 50 50 100 100;;

(*2.2*)

let segment(x,y) (z,t) =
  moveto x y;
  lineto z t;;

let rec montagne (x,y) (z,t) n =
  clear_graph();
  if n = 0 then 
    segment (x,y) (z,t)
  else
    let m = ((x+z)/2) and h = ((y+t) + int(10*n)) in
    begin
	  montagne (x,y) (m,h) (n-1);
	  montagne (m,h) (z,t) (n-1)
    end;;

  (*montagne (20,50) (70,90) 2;;*)

(*2.3*)
  
let rec dragon (x,y) (z,t) n =
   if n = 1 then
     begin
       moveto x y;
       lineto z t;
     end
   else
        let u = ((x+z)/2+(t-y)/2) and v = ((y+t)/2)-((z-x)/2) in
	begin 
	       dragon (x,y) (u,v)  (n-1);
	       dragon (z,t) (u,v)  (n-1);
        end;;

  (*dragon (100,200)(150,110) 17 ;;*)  

(*2.5*)

let triangle (x,y) (z,t) (u,v)  = 
      moveto x y;
      lineto z t;
      lineto u v;
      lineto x y;;

let milieu (x,y) (z,t) =
  (((x+z)/2),((y+t)/2));;

let rec sierp (x,y) (z,t) (u,v) n = match n with
	| 0 ->  triangle (x,y) (z,t) (u,v);
	| n ->  let m1 = milieu (x,y) (z,t) and m2 = milieu (x,y) (u,v) and m3 = milieu (z,t) (u,v) in 			
	   begin
	          sierp (x,y) m1 m2 (n-1);
	          sierp (z,t) m1 m3 (n-1);
	          sierp (u,v) m2 m3 (n-1)
	     end;;

  (*sierp (100,50) (750,50) (450,550) 12;*)
