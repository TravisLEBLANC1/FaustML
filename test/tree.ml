type tree = L | N of tree*tree
type abr = La | N of int*abr*abr
type nat = Z | S of nat
type boolean = True | False 


(*tier: i,j->j with i > j*)
let add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 

let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

(*tier: i,j->k with i,j > k*)
let mul(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add(y, mul(x2,y))

let create(n) = match n with 
  | Z -> L
  | S(m) -> let t = create(m) in N(t,t)
  
let idnat(n) = match n with 
  | S(n1) -> S(idnat(n1))
  | Z -> Z

let idnat2(n) = match n with 
  | S(n1) -> S(idnat2(n1))
  | Z -> Z

let ifelse(b,e1,e2) = match b with 
  | True -> e1 
  | False -> e2 
  | Z -> True 
  | S(x2) -> False

  let leq(a,b) = iszero(sub(b,a))
(*i,i->i*)
let max(x,y) = ifelse(leq(x,y),y,x)

let height(t) = match t with 
  | L -> Z
  | N(t1,t2) -> S(max(height(t1), height(t2)))

let reverse(t) = match t with 
  | L -> L 
  | N(t1,t2) -> N(reverse(t2), reverse(t1))

