type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist
type natlistpair = Pair of natlist*natlist
type boolean = True | False 

let rec sort(l) = 
  let n = lstlength(l) in 
  match sort_aux(n,l) with 
    | Pair(s,l1) -> s 

and sort_aux(n,l) = match n with 
  | Z -> Pair(Nil,l)
  | S(m) -> 
    match sort_aux(m,l) with 
    | Pair(s,l1) ->  match l1 with 
      | Nil -> Pair(s,l1)
      | Cons(x,l11) -> Pair(insert(s,x),l11)

and insert(l,x) = match l with 
  | Nil -> Cons(x,Nil)
  | Cons(y,l1) -> ifelse(leq(x,y),Cons(x,Cons(y,l1)), Cons(y,insert(l1,x)))

and leq(a,b) = iszero(sub(b,a))

and iszero(x) = match x with
  | Z -> True 
  | S(x2) -> False

and pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

(* y - x
  tier: i,j->j with i>j*)
and sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

and lstlength(l) = match l with 
  | Nil -> Z 
  | Cons(_,l1) -> S(lstlength(l1))

and main() = 
  let l = Cons(S(S(S(S(S(Z))))),Cons(S(Z),Cons(S(S(Z)),Cons(S(S(S(S(S(S(S(Z))))))),Cons(S(S(S(S(S(S(S(S(Z)))))))),Nil))))) in 
  sort(l)
