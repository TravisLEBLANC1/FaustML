type tree = L | B of tree * tree | C of int
type int = Z | S of int 

let g(t) = match t with
  | L -> L 
  | B(t1,t2) -> g(t1)
  | C(y) -> f(y)


let f(x) = match x with 
  | Z -> L
  | S(y) -> f(y)

