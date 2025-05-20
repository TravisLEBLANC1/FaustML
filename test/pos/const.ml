type tree = L | B of tree * tree | C of nat
type nat = Z | S of nat 

(* *)
let rec g(t,w) = match t with
  | B(t1,t2) -> g(t1,w)
  | L -> L 
  | C(y) -> f(y,w)


(*i,i->j with i>j*)
and f(x,w) = match x with 
  | Z -> h(w)
  | S(y) -> f(y,w)

and h(x) = C(Z)

