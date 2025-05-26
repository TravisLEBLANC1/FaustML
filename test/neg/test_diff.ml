type nat = Z | S of nat

(*in this example, if the constraints are not well done in recursive calls
  h could pass the tiering
  when it should not, because of this (-) line *)
let id(z) = match z with 
  | Z -> Z 
  | S(q) -> S(id(q))

let f1(x,z) = match x with 
  | Z -> Z
  | S(y) -> S(f2(y,z))

let f2(x,z) = match x with 
  | Z -> id(z)    (*  (-)  *)
  | S(y) -> S(f1(y,z))


let h(x) = match x with 
  | Z -> Z 
  | S(y) -> f1(Z,h(y))
