type ad = AL | A of ad*ba 
type ba = BL | B of ad 
type nat = Z | S of nat


let rec rabbits(n) = match n with 
  | Z -> BL 
  | S(m) -> baby(m)

and baby(n) = match n with 
  | Z -> BL 
  | S(m) -> B(adult(m))
  
and adult(n) = match n with 
  | Z -> AL 
  | S(m) -> A(adult(m), baby(m))

and main(n) = rabbits(n)