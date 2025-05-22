type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist

let sort(l) = match l with 
  | Nil -> Nil 
  | Cons(x,l1) -> insert(sort(l1),x)

let insert(l,x) = match l with 
  | Nil -> Cons(x,Nil)
  | Cons(y,l1) -> 
    if leq(x,y) then 
      Cons(x,Cons(y,l1))
    else 
      Cons(y,insert(l1,x))

let leq(a,b) = iszero(sub(b,a))

let iszero(x) = match x with
  | Z -> true 
  | S(x2) -> false

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

(* y - x
  tier: i,j->j with i>j*)
let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

