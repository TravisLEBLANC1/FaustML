type tree = Lv of alist | N of alist*tree*tree
type nat = Z | S of nat
type alist = R of alist | L of alist | C

(* 
parameter substitution + tree algebra leads outside of poly time
(satisfy typing/tiering but not syntactic restriction)
*)

let rec create(n) = match n with 
  | Z -> Lv(C)
  | S(m) -> let t = create(m) in N(C,t,t)

(* this map function prevent sharing by giving a unique id to each node*)
and map(t,l) = match t with 
  | Lv(_) -> Lv(l)
  | N(_,t1,t2) -> 
    let t1' = map(t1,L(l)) in 
    let t2' = map(t2,R(l)) in 
    N(l,t1',t2')

and traversal(t) = match t with 
  | Lv(l) -> Lv(l)
  | N(l,t1,t2) -> N(l,traversal(t1),traversal(t2))
  
and main(n) = traversal(map(create(n),C))
