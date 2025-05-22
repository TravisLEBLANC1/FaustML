type tree = L | N of nat*tree*tree
type bst = Lbst | Nbst of nat*bst*bst
type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist
type infnat = Nat of nat | NInf | PInf
type boolnatpair = Pair of bool*infnat*infnat

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

let iszero(x) = match x with 
  | Z -> true 
  | S(x2) -> false

let _and(b1,b2) = if b1 then b2 else false

let _not(b) = match b with 
  | True -> False 
  | False -> True

let leq(a,b) = iszero(sub(b,a))

let leq_inf(a,b) = match a with 
  | Nat(an) -> leq(an,b)
  | NInf -> True 
  | PInf -> False 

let lt_inf(a,b) = match b with 
  | Nat(bn) -> lt(a,bn)
  | NInf -> False 
  | PInf -> True

let lt(a,b) = _and(leq(a,b),_not(leq(b,a)))

let eq(a,b) = _and(leq(b,a),leq(a,b))

(*i,i->i*)
let max(x,y) = if leq(x,y) then y else x

(****** list operations *******)

let concat(l1,l2) = match l1 with 
  | Nil -> l2
  | Cons(x,l1') -> Cons(x,concat(l1',l2))

(****** classical trees *******)

let create(n) = match n with 
  | Z -> L
  | S(m) -> let t = create(m) in N(Z,t,t)

let height(t) = match t with 
  | L -> Z
  | N(x,t1,t2) -> S(max(height(t1), height(t2)))

let reverse(t) = match t with 
  | L -> L 
  | N(x,t1,t2) -> N(x,reverse(t2), reverse(t1))

let addalltree(t,c) = match t with 
  | L -> L 
  | N(x,t1,t2) -> N(add(x,c),addalltree(t1,c),addalltree(t2,c))




(****** BST trees *******)

let getval(t) = match t with 
  | Lbst -> Z
  | Nbst(v1,t1,t2) -> v1 


let bstsearch(t,n) = match t with 
  | Lbst -> False 
  | Nbst(m,t1,t2) -> 
    if leq(n,m) then 
      if leq(m,n) then 
        True 
      else
        bstsearch(t1,n)
    else
      bstsearch(t2,n)

let bstinsert(t,n) = match t with 
  | Lbst -> Nbst(n,Lbst,Lbst)
  | Nbst(m,t1,t2) ->
    match leq(n,m) with 
      | True -> Nbst(m,bstinsert(t1,n),t2)
      | False -> Nbst(m,t1,bstinsert(t2,n))

let bstgetmax(t) = match t with 
  | Lbst -> Z (*error*)
  | Nbst(n,t1,t2) -> match t2 with 
    | Lbst -> n
    | Nbst(_,_,_) -> bstgetmax(t2)

let bstextractmin(t) = match t with 
  | Lbst -> Lbst
  | Nbst(n,t1,t2) -> match t1 with 
    | Lbst -> t1
    | Nbst(_,_,_) -> Nbst(n,bstextractmin(t1),t2)

let bstgetmin(t) = match t with 
  | Lbst -> Z (*error*)
  | Nbst(n,t1,t2) -> match t1 with 
    | Lbst -> n
    | Nbst(_,_,_) -> bstgetmin(t1) 

(* let bstremove(t,n) = match t with
  | Lbst-> Lbst 
  | Nbst(m,t1,t2) ->
    match leq(n,m) with 
      | True -> (match leq(m,n) with 
        | True -> let newm = bstgetmin(t2) in (
          match newm with 
            | Z -> t1 
            | S(_) -> Nbst(newm, t1, bstextractmin(t2))) 
        | False -> Nbst(m, bstremove(t1,n), t2))
      | False -> Nbst(m, t1, bstremove(t2,n)) *)


 let bstremove(t,n) = match t with
  | Lbst-> Lbst 
  | Nbst(m,t1,t2) ->
    if leq(n,m) then 
      if leq(m,n) then
        let newm = bstgetmin(t2) in (
          match newm with 
            | Z -> t1 
            | S(_) -> Nbst(newm, t1, bstextractmin(t2))) 
      else 
        Nbst(m,bstremove(t1,n),t2)
    else
      Nbst(m,t1,bstremove(t2,n)) 


let getmin(p,n) =match p with 
  | Pair(_,m,_) -> 
    match m with 
      | Nat(m1) -> m
      | NInf -> Nat(n) 
      | PInf -> Nat(n) 
  

let getmax(p,n) = match p with 
  | Pair(_,_,m) ->
    match m with 
      | Nat(m1) -> m
      | NInf -> Nat(n)
      | PInf -> Nat(n) 

let getbool(p) = match p with 
  | Pair(b,_,_) -> b

let conjpair(p1,p2) = 
  let b1 = getbool(p1) in 
  let b2 = getbool(p2) in 
  _and(b1, b2)

let isvalidpair(n,p1,p2) = match p1 with 
  | Pair(_,_,m1) -> match p2 with 
    | Pair(_,n2,_) -> _and(leq_inf(m1,n), lt_inf(n,n2))


let isbst_aux(t) = match t with 
  | Lbst -> Pair(True, PInf, NInf)
  | Nbst(n,t1,t2) -> 
    let p1 = isbst_aux(t1) in 
    let p2 = isbst_aux(t2) in 
    if isvalidpair(n,p1,p2) then 
      Pair(conjpair(p1,p2),getmin(p1,n),getmax(p2,n))
    else
      Pair(False, getmin(p1,n),getmax(p2,n))
      
      
let isbst(t) = getbool(isbst_aux(t))

let testbst() = 
  let ten = S(S(S(S(S(S(S(S(S(S(Z)))))))))) in 
  let eleven = S(ten) in 
  let twelve = S(eleven) in
  let five = S(S(S(S(S(Z)))))in 
  let two = S(S(Z)) in 
  Nbst(ten, Nbst(five, Nbst(two,Lbst,Lbst), Nbst(twelve, Lbst,Lbst)), Nbst(eleven,Lbst,Lbst))

let testbst1() = 
  let ten = S(S(S(S(S(S(S(S(S(S(Z)))))))))) in 
  let eleven = S(ten) in 
  let twelve = S(eleven) in
  let five = S(S(S(S(S(Z)))))in 
  let two = S(S(Z)) in 
  Nbst(ten, Nbst(five, Nbst(two,Lbst,Lbst), Lbst), Nbst(twelve,Nbst(eleven, Lbst,Lbst),Lbst))

let main() = 
  let t=testbst1() in 
  isbst(t)