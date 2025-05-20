type nat = Z | S of nat

let rec cube(x) = mul(x,mul(x,x))

and add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 

and mul(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add(y, mul(x2,y))

and main(x) = cube(x)

