type nat = Z | S of nat

(*i,j->k avec i>j et j>i 
3,4   4 > 3   3 2  avec 3 > 2  
*)
let rec eq(a,b) = _and(leq(b,a),leq(a,b))

(*i,j->k avec j>i 
i,j->k avec ilexist q j >q*)
and leq(a,b) = iszero(sub(b,a))



and _and(b1,b2) = if b1 then b2 else false

(*i->j*)
and iszero(x) = match x with
  | Z -> true 
  | S(x2) -> false

(*i,j->j avec i > j
ou i,j->k avec i>k et j>=k
*)
and sub(x,y) = match x with
  | Z -> y 
  | S(x2) -> pred(sub(x2,y)) 

(*i->i ou i->j avec i>=j*)
and pred(x) = match x with 
  | Z -> Z 
  | S(y) -> y

and main(a,b) = eq(a,b)