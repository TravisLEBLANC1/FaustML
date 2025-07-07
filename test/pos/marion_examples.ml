type nat = Z | S of nat
type natlist = Nil | Cons of nat*natlist
type binstring = Eps | One of binstring | Zer of binstring

(********** listInf **********)

let inf(x,y) = match x with 
  | Z -> Z 
  | S(x1) -> match y with 
    | Z -> Z 
    | S(y1) -> S(inf(x1,y1))

let listInf(l) = match l with 
  | Nil -> Z 
  | Cons(n,l1) -> inf(n,listInf(l1))

(**********  insert sort ***********)
let pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

let sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

let leq(a,b) = iszero(sub(b,a))

let iszero(x) = match x with
  | Z -> true 
  | S(x2) -> false

let length(l) = match l with 
  | Nil -> Z 
  | Cons(_,l1) -> S(length(l1))

let sort_intern(n,l) = match n with 
  | Z -> l
  | S(m) -> match l with 
    | Nil -> Nil 
    | Cons(x,l1) -> insert(m,sort_intern(m,l1),x) (* param subst *)

let insert(n,l,x) = match n with 
  | Z -> Cons(x,Nil)
  | S(m) -> match l with 
    | Nil -> Cons(x,Nil)
    | Cons(y,l1) -> 
      if leq(x,y) then 
        Cons(x,Cons(y,l1))
      else 
        Cons(y,insert(m,l1,x))

let sort(l) = sort_intern(length(l),l)



(**********   longest common sequence  *************)

let max(m,x,y) = 
match m with 
  | Z -> Z (*m not big enough*)
  | S(m1) ->
  match x with 
    | Z -> y 
    | S(x1) -> match y with 
      | Z -> x 
      | S(y1) -> S(max(m1,x1,y1))

let lcs(m,x,y) = 
match m with 
  | Z -> Z (*m not big enough*)
  | S(m1) ->
  match x with 
    | Eps -> Z
    | One(x1)-> (match y with 
      | Eps -> Z 
      | One(y1) -> S(lcs(m1,x1,y1))
      | Zer(y1) -> max(m,lcs(m1,x1,y), lcs(m1,x,y1)))
    | Zer(x1) -> (match y with 
      | Eps -> Z 
      | One(y1) -> max(m,lcs(m1,x1,y), lcs(m1,x,y1))
      | Zer(y1) ->  S(lcs(m1,x1,y1)))

let lengthb(l) = match l with 
  | Eps -> Z 
  | One(l1) -> S(lengthb(l1))
  | Zer(l1) -> S(lengthb(l1))

let add(x,y) = match x with 
  | Z -> y
  | S(x2) -> S(add(x2,y)) 

let main(l1,l2) = 
  let m1 = lengthb(l1) in 
  let m2 = lengthb(l2) in 
  lcs(add(m1,m2), l1,l2)