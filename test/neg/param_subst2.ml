type tree = Lv | N of tree*tree
type nat = Z | S of nat
type bitlist = Zero of bitlist | One of bitlist | Nil  

(* 
parameter substitution + tree algebra leads outside of poly time
(satisfy typing/tiering but not syntactic restriction)
this time we don't prevent sharing, we prevent memoization
*)

let rec create(n) = match n with 
  | Z -> Lv
  | S(m) -> let t = create(m) in N(t,t)

(* this traverse function prevent memoization by giving a unique id to each call*)
and traverse(t,l) = match t with 
  | Lv -> Lv
  | N(t1,t2) -> 
    let t1' = traverse(t1,Zero(l)) in 
    let t2' = traverse(t2,One(l)) in 
    N(t1',t2')

  
and main(n) = traverse(create(n),Nil)
