type nat = Z | S of nat
type tree = L | N of nat*tree*tree


(* i->i*)
let rec pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

(* i,j->k with i>k and j>k*)
and sub(x,y) = match x with
  | Z -> id(y) 
  | S(x2) -> pred(sub(x2,y)) 

(* i-> k with i>k*)
and id(x) = match x with 
  | Z -> Z 
  | S(y) -> S(id(y))

(*i->k with no restrictions*)
and iszero(x) = match x with 
  | Z -> true
  | S(x2) -> false

(*i,j->q with exists k. i>k and j>k*)
and leq(a,b) = iszero(sub(b,a))

(*i,i->i  with exists k. i>k *)
and max(x,y) = if leq(x,y) then y else x

(*i->j with exists k. i>j>k*)
and height(t) = match t with 
  | L -> Z
  | N(x,t1,t2) -> S(max(height(t1), height(t2)))

and main(t) = height(t)