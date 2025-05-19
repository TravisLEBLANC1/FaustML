type nat = Z | S of nat 

let fact(x)= match x with 
  | Z -> S(Z)
  | S(x') -> mul(x,fact(x'))
  
let add(x,y) = match x with 
  | Z -> y
  | S(x') -> S(add(x',y)) 
let mul(x,y) = match x with 
  | Z -> Z
  | S(x') -> add(y, mul(x',y))
    
