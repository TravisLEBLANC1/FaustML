type lst = Nil | Cons of nat * lst
type nat = Z | S of nat
type mod5 =  Four | Three | Two | One | Zero
type tree = L | N of nat*tree*tree

let mod5succ(m) = match m with 
  | Four -> Zero 
  | Three -> Four 
  | Two -> Three 
  | One -> Two 
  | Zero -> One

let mod5length(l,m) = match l with 
  | Nil -> m 
  | Cons(_,l1) -> mod5length(l1,mod5succ(m))

let _and(b1,b2) = if b1 then b2 else false

let eq(a,b) = match a with 
  | Z -> iszero(b)
  | S(a1) -> match b with 
    | Z -> false 
    | S(b1) -> eq(a1,b1)

let iszero(x) = match x with
  | Z -> true 
  | S(x2) -> false

let zero_before_one(t,b) = match t with 
  | L -> true 
  | N(n,l,r) -> 
    if _and(b,eq(n,Z)) then 
      false 
    else if eq(n,S(Z)) then 
      _and(zero_before_one(l,true), zero_before_one(r,true))
    else
      _and(zero_before_one(l,b), zero_before_one(r,b))


let main(t) = zero_before_one(t,false)