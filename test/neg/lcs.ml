type binstring = Eps | One of binstring | Zer of binstring
type nat = Z | S of nat

(* this version of lcs (longest common sequence) would require some kind of generalization,
because lcs decrease "one" of two argument, 
wheras we only allow to decrease "always the first one and maybe the others" *)

let rec max(b,x,y) = match x with 
  | Z -> y 
  | S(x1) -> match y with 
    | Z -> x 
    | S(y1) -> match b with 
      | Eps -> Z (* error? *)
      | One(b1) -> S(max(b1,x1,y1))
      | Zer(b1) -> S(max(b1,x1,y1))


and lcs(x,y) = match x with 
  | Eps -> Z
  | One(x1)-> (match y with 
    | Eps -> Z 
    | One(y1) -> S(lcs(x1,y1))
    | Zer(y1) -> max(x,lcs(x1,y), lcs(x,y1)))
  | Zer(x1) -> (match y with 
    | Eps -> Z 
    | One(y1) -> max(x,lcs(x1,y), lcs(x,y1)) (*lcs(x,y1) not allowed*)
    | Zer(y1) ->  S(lcs(x1,y1)))