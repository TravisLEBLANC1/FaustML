type ad = AL | A of ad*ba 
type ba = BL | B of ad 
type nat = Z | S of nat


let rabbits(n) = match n with 
  | Z -> BL 
  | S(m) -> baby(m)

let baby(n) = match n with 
  | Z -> BL 
  | S(m) -> B(adult(m))
  
let adult(n) = match n with 
  | Z -> AL 
  | S(m) -> A(adult(m), baby(m))

