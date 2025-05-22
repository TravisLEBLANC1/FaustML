type bit = One | Zer 
type num = Eps | Bit of bit * num
type bitlist = Nil | Cons of bit*bitlist 
type boolean = True | False 

let neg(b) = match b with 
  | One -> Zer 
  | Zer -> One 

let iseps(n) = match n with 
  | Eps -> True 
  | Bit(_,_) -> False

let _and(b1,b2) = match b1 with 
  | False -> False
  | True -> id(b2)

let eqbit(b1,b2) =  match b1 with 
  | One -> (match b2 with 
    | One -> True 
    | Zer -> False)
  | Zer -> match b2 with 
    | One -> False 
    | Zer -> True 

let eq(n,m) = match n with 
  | Eps -> iseps(m)
  | Bit(bn,n1) -> (match nm with 
    | Eps -> False 
    | Bit(bm,m1) -> match eqbit(bn,bm) with 
      | True -> eq(n1,m1))
  | Zer -> match b2 with 
    | One -> False 
    | Zer -> True 
  
let member(l,x) = match l with 
  | Nil -> False 
  | Cons(b,ls) -> match eq(b,x) with 
    | True -> True 
    | False -> member(ls,x)

let consistent(l) = match l with 
  | Nil -> True 
  | Cons(b,ls) -> match member(ls,neg(b)) with 
    | True -> False 
    | False -> consistent(ls)

