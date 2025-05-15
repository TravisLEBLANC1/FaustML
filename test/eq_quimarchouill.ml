type nat = Z | S of nat
type boolean = True | False 


let eq(a,b) = _and(leq(b,a),leq(a,b))

let leq(a,b) = iszero(sub(b,a))



let _and(b1,b2) = match b1 with 
  | False -> False
  | True -> (match b2 with 
    | True -> True 
    | False -> False)

let iszero(x) = match x with
  | Z -> True 
  | S(x2) -> False


let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

let idnat(n) = match n with 
  | S(n1) -> S(idnat(n1))
  | Z -> Z

let idnat2(n) = match n with 
  | S(n1) -> S(idnat2(n1))
  | Z -> Z