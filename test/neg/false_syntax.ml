type tree = Lv | N of tree*tree
type nat = Z | S of nat

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y


let traverse(t,l,m) = match t with 
  | Lv -> Lv
  | N(t1,t2) -> 
    let t1' = traverse(t1,m,pred(l)) in (*exchange of argument not allowed*)
    let t2' = traverse(t2,l,m) in 
    N(t1',t2')


