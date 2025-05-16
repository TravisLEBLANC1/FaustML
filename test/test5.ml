
type tree = L | N of tree * tree 
type nat = Z | S of nat
let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

(*i->i ou i->j avec i>=j*)
let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y