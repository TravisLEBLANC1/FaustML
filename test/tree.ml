type tree = L | N of tree*tree
type bst = Lbst | Nbst of nat*abr*abr
type nat = Z | S of nat
type boolean = True | False 
type infnat = Nat of nat | Inf | MInf 
type boolnatpair = Pair of boolean*infnat*infnat

(********* intergers ********)
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

let ifelse(b,e1,e2) = match b with 
  | True -> e1 
  | False -> e2 

let iszero(x) = match x with 
  | Z -> True 
  | S(x2) -> False

let _and(b1,b2) = match b1 with 
  | False -> False
  | True -> b2

let leq(a,b) = iszero(sub(b,a))

let eq(a,b) = _and(leq(b,a),leq(a,b))

(*i,i->i*)
let max(x,y) = ifelse(leq(x,y),y,x)

(****** classical trees *******)

let create(n) = match n with 
  | Z -> L
  | S(m) -> let t = create(m) in N(t,t)

let height(t) = match t with 
  | L -> Z
  | N(t1,t2) -> S(max(height(t1), height(t2)))

let reverse(t) = match t with 
  | L -> L 
  | N(t1,t2) -> N(reverse(t2), reverse(t1))


(****** BST trees *******)

let getval(t) = match t with 
  | Lbst -> Z
  | Nbst(v1,t1,t2) -> v1 

let bstinsert(t,n) = 
  match t with 
    | Lbst -> Nbst(n,Lbst,Lbst)
    | Nbst(m,t1,t2) -> 
      ifelse(leq(n,m), 
        Nbst(m,bstinsert(t1,n),t2), 
        Nbst(m,t1,bstinsert(t2,n)))

