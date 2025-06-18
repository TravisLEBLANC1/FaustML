type tree = L | N of nat*tree*tree
type nat = Z | S of nat

let _and(b1,b2,b3) = 
  if b1 then (if b2 then (if b3 then true else false) else false) else false


let isleave(t) = match t with 
  | L -> true 
  | N(_,_,_) -> false

let iszero(x) = match x with
  | Z -> true 
  | S(x2) -> false

let eq(a,b) = match a with 
  | Z -> iszero(b)
  | S(a1) -> match b with 
    | Z -> false 
    | S(b1) -> eq(a1,b1)


let ismirror(t1,t2) = match t1 with 
  | L -> isleave(t2)
  | N(v1,t11,t12) -> match t2 with 
    | L -> false 
    | N(v2,t21,t22) -> 
      _and(eq(v1,v2), ismirror(t11,t22), ismirror(t12,t21))


let main() = 
  let five = S(S(S(S(S(Z))))) in 
  let three = S(S(S(Z))) in 
  let one = S(Z) in 
  let t1 = N(one,N(five, L,L), N(three,N(five,L,L),L)) in
  let t2 = N(one,N(three, L,N(five,L,L)), N(five,L,L)) in
  ismirror(t1,t2) 