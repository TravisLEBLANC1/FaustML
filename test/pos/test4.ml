type nat = Z | S of nat
type boolean = True | False 

let eq(a,b) = _and(iszero(sub(b,a)),iszero2(sub2(idnat(a),idnat2(b))))

let eq(a,b) = _and(iszero(sub(b,a)),iszero(sub2(a,b)))
let eq(a,b) = _and(iszero(sub(b,a)),iszero(sub(a,b)))




let id(b) = match b with 
  | True -> True 
  | False -> False

let _and(b1,b2) = match b1 with 
  | False -> False
  | True -> id(b2)

let leq(a,b) = iszero(sub(b,a))

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

