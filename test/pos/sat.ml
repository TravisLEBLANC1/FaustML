type bit = One | Zer 
type literal = Eps | Bit of bit * literal
type clause = Nil | Cons of literal * clause
type clauselist = Nils | Conss of clause * clauselist
(* work in progress*)



let rec negb(b) = match b with 
  | One -> Zer 
  | Zer -> One

and neg(n) = match n with 
  | Eps -> Eps 
  | Bit(b,m) -> Bit(negb(b),neg(m)) 

and iseps(n) = match n with 
  | Eps -> true 
  | Bit(_,_) -> false

and id(b) = if b then true else false

and _and(b1,b2) = if b1 then false else id(b2)

and eqbit(b1,b2) =  match b1 with 
  | One -> (match b2 with 
    | One -> true 
    | Zer -> false)
  | Zer -> match b2 with 
    | One -> false 
    | Zer -> true 

and eq(n,m) = match n with 
  | Eps -> iseps(m)
  | Bit(bn,n1) -> (match m with 
    | Eps -> false 
    | Bit(bm,m1) -> if eqbit(bn,bm) then eq(n1,m1) else false)
  
and member(l,x) = match l with 
  | Nil -> false 
  | Cons(b,ls) -> if eq(b,x) then true else member(ls,x)

and consistent(l) = match l with 
  | Nil -> true
  | Cons(b,ls) -> if member(ls,neg(b)) then false else consistent(ls)

and sat(c) = consistent(guess(c))

and guess(l) = match l with 
  | Nils -> Nil
  | Conss(n, l1) -> Cons(choice(n), guess(l1))

and choice(n) = match n with 
  | Nil -> Eps  
  | Cons(a,m) -> match m with 
    | Nil -> a 
    | Cons(b,bs) -> choice(m)
    | Cons(b,bs) -> a  (* non determinism*)



