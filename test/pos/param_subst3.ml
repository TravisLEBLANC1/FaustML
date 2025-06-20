type tree = L | N of tree*tree
type nat = Z | S of nat
type bitlist = Zero of bitlist | One of bitlist | Nil  

(* 
parameter **subterm** substitution + tree algebra DOES NOT leads outside of poly time
*)

let rec create(n) = match n with 
  | Z -> L
  | S(m) -> let t = create(m) in N(t,t)

(* this is an attempt to create unique identifier with **subterm** substitution
but their is still a lot of sharing possible*)
and traverse(t,n) = match t with 
  | L -> L
  | N(t1,t2) -> 
    let t1' = traverse(t1,pred(n)) in 
    let t2' = traverse(t2,n) in 
    N(t1',t2')

and pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

and sub(x,y) = match x with
  | Z -> id(y) 
  | S(x2) -> pred(sub(x2,y)) 

  and id(x) = match x with 
  | Z -> Z 
  | S(y) -> S(id(y))

and iszero(x) = match x with 
  | Z -> true
  | S(x2) -> false

and leq(a,b) = iszero(sub(b,a))

and max(x,y) = if leq(x,y) then y else x

and height(t) = match t with 
  | L -> Z
  | N(t1,t2) -> S(max(height(t1), height(t2)))

and main(n) = 
  let t = create(n) in 
  traverse(t,height(t))
