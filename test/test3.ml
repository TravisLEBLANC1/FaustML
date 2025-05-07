(*    
let ftype = (["tree"; "int"], "int") in 
let gtype = (["tree"; "int"], "int") in
let htype = (["int";"int"], "int") in 
let ttype = (["int"],"int") in 
Typecheck.typ_prog prog (StringMap.add "t" ttype @@ StringMap.add "h" htype @@ StringMap.add "g" gtype @@ StringMap.singleton "f" ftype);
*)
type tree = N | B of tree * tree
type int = Z | S of int 
let f(x,y) = match x with
    | N -> y
    | B(x1,x2) -> t(h(f(x1,y), g(x2,y)))

let g(x,y) = match x with
    | N -> y
    | B(x1,x2) -> t(h(g(x2,y), f(x1,y)))

let h(x,y) = Z
let t(x) = Z