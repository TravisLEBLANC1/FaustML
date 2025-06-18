type nat = Z | S of nat
type boollist = Nil | Cons of bool*boollist
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

let eq2(a,b) = match a with 
  | Z -> iszero(b)
  | S(a1) -> match b with 
    | Z -> false 
    | S(b1) -> eq2(a1,b1)

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

let main(x) = iseven(x)


(***** list operations ********)

let tl(l) = match l with 
  | Nil -> Nil 
  | Cons(b,l') -> l' 

let hd(l) = match l with 
  | Nil -> false 
  | Cons(b,l') -> b 

let at_intern(i,l) = match i with 
  | Z -> l
  | S(j) -> tl(at_intern(j,l))

let at(i,l) = hd(at_intern(i,l))

let concat(l1,l2) = match l1 with 
  | Nil -> l2
  | Cons(x,l1') -> Cons(x,concat(l1',l2))

