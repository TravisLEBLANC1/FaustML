type bool = T | F
type nat = Z | S of nat 

let twice(n) = match n with 
  | Z -> Z 
  | S(m) -> S(S(twice(m)))

(* return 2^n (not tierable because expR > expR)*)
let exp(n) = match n with 
  | Z -> S(Z)
  | S(m) -> twice(exp(m))