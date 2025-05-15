type nat = Z | S of nat

let cube(x) = mul(x,mul2(idnat1(x),idnat2(x)))

let idnat1(n) = match n with 
  | S(n1) -> S(idnat1(n1))
  | Z -> Z

let idnat2(n) = match n with 
  | S(n1) -> S(idnat2(n1))
  | Z -> Z

let add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 

let add2(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add2(x2,y)) 

let mul(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add(y, mul(x2,y))

let mul2(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add2(y, mul2(x2,y))

