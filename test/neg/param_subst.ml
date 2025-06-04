type tree = Lv of bitlist | N of bitlist*tree*tree
type nat = Z | S of nat
type bitlist = Zero of bitlist | One of bitlist | Nil  

(* 
parameter substitution + tree algebra leads outside of poly time
(satisfy typing/tiering but not syntactic restriction)
*)

let rec create(n) = match n with 
  | Z -> Lv(Nil)
  | S(m) -> let t = create(m) in N(Nil,t,t)

(* this map function prevent sharing by giving a unique id to each node*)
and map(t,l) = match t with 
  | Lv(_) -> Lv(l)
  | N(_,t1,t2) -> 
    let t1' = map(t1,Zero(l)) in 
    let t2' = map(t2,One(l)) in 
    N(l,t1',t2')

and traversal(t) = match t with 
  | Lv(l) -> Lv(l)
  | N(l,t1,t2) -> N(l,traversal(t1),traversal(t2))
  
and main(n) = traversal(map(create(n),Nil))
