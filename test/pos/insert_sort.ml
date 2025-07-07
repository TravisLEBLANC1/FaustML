type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist

(* 
  this version does not pass the simrec syntax restriction, but it does satisfy tiering
  for this version to be accepted we need some kind of parameter substitution
*)

let sort_intern(n,l) = match n with 
  | Z -> l
  | S(m) -> match l with 
    | Nil -> Nil 
    | Cons(x,l1) -> insert(m,sort_intern(m,l1),x) (* param subst *)

let insert(n,l,x) = match n with 
  | Z -> Cons(x,Nil)
  | S(m) -> match l with 
    | Nil -> Cons(x,Nil)
    | Cons(y,l1) -> 
      if leq(x,y) then 
        Cons(x,Cons(y,l1))
      else 
        Cons(y,insert(m,l1,x))

let sort(l) = sort_intern(length(l),l)

let length(l) = match l with 
  | Nil -> Z 
  | Cons(_,l1) -> S(length(l1))

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

let main() = 
  let l = Cons(S(S(S(S(S(Z))))),Cons(S(Z),Cons(S(S(Z)),Cons(S(S(S(S(S(S(S(Z))))))),Cons(S(S(S(S(S(S(S(S(Z)))))))),Nil))))) in 
  sort(l)
