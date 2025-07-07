type nat = Z | S of nat
type boollist = Nil | Cons of bool*boollist
type natlist = E | C of nat*natlist
type formulae = 
    Bool of bool
  | And of formulae*formulae
  | Or of formulae*formulae
  | Not of formulae
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
  | Not(f1) -> 
    let b = istrue(f1) in 
    _not(b)

(*tier: i->k*)
let id(b) = if b then true else false

let forall(blist) = match blist with 
  | Nil -> true 
  | Cons(b, blist2) -> _and(b,forall(blist2))

let exists(blist) = match blist with 
  | Nil -> false 
  | Cons(b, blist2) -> _or(b,exists(blist2))


(* logical and 
   tier: i,j -> k *)
let _and(b1,b2) = if b1 then b2 else false

(* logical or 
   tier: i,j -> k   *)
let _or(b1,b2) = if b1 then true else b2

let _not(b) = if b then false else true

(* you are not intended to do a match on bool
   but if you don't want to use if-else construction
   the syntax bellow is accepted :*)
let ifelse(b,e1,e2) = match b with 
  | True -> e1 
  | False -> e2 

(******** Integer operations ********)

let iseven(x) = match x with 
  | Z -> true
  | S(y) -> isodd(y)

let isodd(y) = match y with 
  | Z -> false 
  | S(x) -> iseven(x)

let eq(a,b) = match a with 
  | Z -> iszero(b)
  | S(a1) -> match b with 
    | Z -> false 
    | S(b1) -> eq(a1,b1)

let leq(a,b) = iszero(sub(b,a))

let lt(a,b) = _and(leq(a,b),_not(leq(b,a)))

let iszero(x) = match x with
  | Z -> true 
  | S(x2) -> false


let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

(*tier: i,j->j with i > j*)
let add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 

(* y - x
  tier: i,j->j with i>j*)
let sub(x,y) = match x with
  | Z -> y
  | S(x2) -> sub(x2,pred(y)) 


(*tier: i,j->k with i,j > k*)
let mul(x,y) = match x with 
  | Z -> Z
  | S(x2) -> add(y, mul(x2,y))

let square(x) = mul(x,x)
let cube(x) = mul(x,mul(x,x))
let power4(x) = mul(mul(x,x),mul(x,x))
let power5(x) = mul(x,power4(x))
let power6(x) = cube(square(x)) 


(***** list operations ********)

let tl(l) = match l with 
  | E -> E
  | C(b,l') -> l' 

let hd(l) = match l with 
  | E -> Z (* error *) 
  | C(b,l') -> b 

let at(i,l) = match i with 
  | Z -> hd(l) 
  | S(j) -> at(j,tl(l))

let concat(l1,l2) = match l1 with 
  | E -> l2
  | C(x,l1') -> C(x,concat(l1',l2))

let add2end(n,l,x) = match n with 
  | Z -> C(x,l)
  | S(n1) -> match l with 
    | E -> C(x,E)
    | C(y,l1) -> C(y,add2end(n1,l1,x))

let length(l) = match l with 
  | E -> Z 
  | C(_,l1) -> S(length(l1))

let reverse(n,l) = match n with 
  | Z -> l 
  | S(n1) -> match l with 
    | E -> E (*error*)
    | C(x,l1) -> add2end(n1,reverse(n1,l1), x)


let main() = 
  let l = C(Z, C(S(Z),C(S(S(Z)), C(S(S(S(Z))), E)))) in 
  at(S(Z),l)
