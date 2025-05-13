type nat = Z | S of nat
type boolean = True | False 


let eq(a,b) = _and(leq(b,a),leq(a,b))

let _and(b1,b2) = match b1 with 
  | False -> False
  | True -> b2

let leq(a,b) = iszero(sub(b,a))

let iszero(x) = match x with
  | Z -> True 
  | S(x2) -> False


let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

