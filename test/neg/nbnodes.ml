type tree = L | N of nat*tree*tree
type nat = Z | S of nat

(* 
this is why it's scary to change the other arguments beside the first one
satisfy typing/tiering but not syntactic restriction
*)

let rec create(n) = match n with 
  | Z -> L
  | S(m) -> let t = create(m) in N(Z,t,t)

and nbnodes(t,n) = match t with 
  | L -> n 
  | N(_,t1,t2) -> 
    let n1 = nbnodes(t1,n) in 
    let n2 = nbnodes(t2,n1) in (* i changed n here*)
    S(n2)

and main(n) = nbnodes(create(n),S(Z))
