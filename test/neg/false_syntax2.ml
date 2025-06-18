type tree = Lv | N of tree*tree
type nat = Z | S of nat
let succ(x) = S(x)

let traverse(t,l,m) = match t with 
  | Lv -> Lv
  | N(t1,t2) -> 
    let t1' = traverse(t1,succ(l),m) in (*not-cons-free function not allowed*)
    let t2' = traverse(t2,l,m) in 
    N(t1',t2')


