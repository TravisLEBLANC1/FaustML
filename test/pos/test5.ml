type boolean = True | False 
type tree = L | N of nat*tree * tree 
type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist

let concat(l1,l2) = match l1 with 
  | Nil -> l2
  | Cons(x,l11) -> Cons(x,concat(l11,l2))

let prefixtraversal(t) = match t with 
  | L -> Nil
  | N(x,t1,t2) -> 
    let left = prefixtraversal(t1) in 
    let right = prefixtraversal(t2) in 
    Cons(x,concat(left, right))