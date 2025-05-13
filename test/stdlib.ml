type boolean = True | False 
type nat = Z | S of nat
type boollist = Nil | Cons of boolean*boollist
type formulae = 
    Bool of boolean
  | And of formulae*formulae
  | Or of formulae*formulae
type tree = L | N of tree*tree



(******** Boolean operations  ********)
let istrue(f) = match f with 
  | Bool(b) -> id(b) 
  | And(f1,f2) -> 
    let b1 = istrue(f1) in
    let b2 = istrue(f2) in 
    _and(b1,b2) 
  | Or(f1,f2) ->
    let b1 = istrue(f1) in
    let b2 = istrue(f2) in 
    _or(b1,b2) 

(*tier: i->k*)
let id(b) = match b with 
  | True -> True 
  | False -> False

let forall(blist) = match blist with 
  | Nil -> True 
  | Cons(b, blist2) -> _and(b,forall(blist2))

let exists(blist) = match blist with 
  | Nil -> False 
  | Cons(b, blist2) -> _or(b,exists(blist2))


(* logical and 
   tier: i,j -> k *)
let _and(b1,b2) = match b1 with 
  | False -> False
  | True -> id(b2)

(* logical or 
   tier: i,j -> k   *)
let _or(b1,b2) = match b1 with 
  | True -> True
  | False -> id(b2)

let _not(b) = match b with 
  | True -> False 
  | False -> True

let ifelse(b,e1,e2) = match b with 
  | True -> e1 
  | False -> e2 

(******** Integer operations ********)


let leq(a,b) = iszero(sub(b,a))
let leq2(a,b) = iszero2(sub2(b,a))


let iszero(x) = match x with
  | Z -> True 
  | S(x2) -> False

let iszero2(x) = match x with
  | Z -> True 
  | S(x2) -> False

let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

let pred2(x) = match x with 
  | Z -> Z 
  | S(y) -> y

(*tier: i->k with i>k*)
let idnat(n) = match n with 
  | S(n1) -> S(idnat(n1))
  | Z -> Z

let idnat2(n) = match n with 
  | S(n1) -> S(idnat2(n1))
  | Z -> Z

let idnat3(n) = match n with 
  | S(n1) -> S(idnat3(n1))
  | Z -> Z


(*tier: i,j->j with i > j*)
let add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 

(*tier: i,j->k with i,j > k*)
let add2(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add2(x2,y)) 

(* y - x
  tier: i,j->j with i>j*)
let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

(* y - x
  tier: i,j->j with i>j*)
let sub2(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred2(sub2(x2,y)) 



(*tier: i,j->k with i,j > k*)
let mul(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add(y, mul(x2,y))

let mul2(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add2(y, mul2(x2,y))

let square(x) = mul(x,x)



(* tier i,j->k with i,j>k*)
