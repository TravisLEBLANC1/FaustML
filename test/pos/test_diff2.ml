type nat = Z | S of nat

(* in this example, my tiering will not give the same tier for f1R and f2R
  but it's still possible. 
  This is an example of an untierable program in SIMREC, 
  which is tierable with mine, but changes f1R and f2R
*)
let id(x) = match x with 
  | Z -> Z 
  | S(y) -> S(id(y))

let ignore(x) = S(S(S(S(Z))))

(*f1:2,2->1*)
let f1(x,z) = match x with 
  | Z -> Z
  | S(y) -> ignore(f2(y,z))


(*f2:2,2->0 *)
let f2(x,z) = match x with 
  | Z -> Z    
  | S(y) -> id(f1(y,z))

