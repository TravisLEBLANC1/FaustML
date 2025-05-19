type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist
type boolean = True | False 

let sort(l) = match l with 
  | Nil -> Nil 
  | Cons(x,l1) -> insert(sort(l1),x)

let insert(l,x) = match l with 
  | Nil -> Cons(x,Nil)
  | Cons(y,l1) -> ifelse(leq(x,y),Cons(x,Cons(y,l1)), Cons(y,insert(l1,x)))

let leq(a,b) = iszero(sub(b,a))

let iszero(x) = match x with
  | Z -> True 
  | S(x2) -> False

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

(* y - x
  tier: i,j->j with i>j*)
let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 




let ifelse(b,e1,e2) = match b with 
  | True -> e1 
  | False -> e2 
