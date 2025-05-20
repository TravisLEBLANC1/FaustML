type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist
type boolean = True | False 

let sort(n,l) = match n with 
  | Z -> Nil
  | S(m) -> match l with 
    | Nil -> Nil 
    | Cons(x,l1) -> insert(m,sort(m,l1),x)

let insert(n,l,x) = match n with 
  | Z -> Cons(x,Nil)
  | S(m) -> match l with 
    | Nil -> Cons(x,Nil) (*error*)
    | Cons(y,l1) -> ifelse(leq(x,y),Cons(x,Cons(y,l1)), Cons(y,insert(m,l1,x)))

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


let main() = 
  let l = Cons(S(S(S(S(S(Z))))),Cons(S(Z),Cons(S(S(Z)),Cons(S(S(S(S(S(S(S(Z))))))),Cons(S(S(S(S(S(S(S(S(Z)))))))),Nil))))) in 
  sort(S(S(S(S(S(Z))))), l)
