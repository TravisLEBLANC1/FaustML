type nat = Z | S of nat

let add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 


let mul(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add(y, mul(x2,y))
let power(n,x) = match n with 
  | Z -> S(Z)
  | S(m) -> mul(x,power(m,x))