type ad = AL | A of ad*ba 
type ba = BL | B of ad 
type nat = Z | S of nat


let rabbits(n) = match n with 
  | Z -> BL 
  | S(m) -> baby(m)

(*i->j avec i>j     2->0*)
let baby(n) = match n with 
  | Z -> BL 
  | S(m) -> B(adult(m))
  
(* i->j avec i>j     2->0*)
let adult(n) = match n with 
  | Z -> AL 
  | S(m) -> A(adult(m), baby(m))

(*
m<=n adult(m) n=m, nb=m  ares<=ares
mb<=nb adult(mb) n=mb bres<=bres
*)