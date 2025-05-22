type nat = Z | S of nat

let eq(a,b) = _and(leq(b,a),leq2(idnat(a),idnat2(b)))


let _and(b1,b2) = if b1 then b2 else false

let leq(a,b) = iszero(sub(b,a))
let leq2(a,b) = iszero2(sub2(b,a))

let iszero(x) = match x with
  | Z -> True 
  | S(x2) -> False

let iszero2(x) = match x with
  | Z -> True 
  | S(x2) -> False

let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

let sub2(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred2(sub2(x2,y)) 
let idnat(n) = match n with 
  | S(n1) -> S(idnat(n1))
  | Z -> Z
let idnat2(n) = match n with 
  | S(n1) -> S(idnat2(n1))
  | Z -> Z

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

let pred2(x) = match x with 
  | Z -> Z 
  | S(y) -> y

