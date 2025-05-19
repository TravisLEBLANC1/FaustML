type tree = L | N of tree * tree 
type nat = Z | S of nat
let add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 

let leafs(t) = match t with 
  | L -> S(Z)
  | N(t1,t2) -> add(leafs(t1),leafs(t2))