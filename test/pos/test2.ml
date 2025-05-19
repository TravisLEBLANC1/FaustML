
(*    
let ftype = (["tree"; "int"], "int") in 
let gtype = (["int"; "tree";"int"], "int") in
let htype = (["int"], "int") in 
let addtype = (["int";"int"], "int") in 
Typecheck.typ_prog prog (StringMap.add "add" addtype @@  StringMap.add "h" htype @@ StringMap.add "g" gtype @@ StringMap.singleton "f" ftype);
*)
type tree = L | B of tree * tree
type int = Z | S of int 

let f(x,y) = match x with 
    | L -> h(y)
    | B(x1,x2) -> g(f(x1,y),x2,y)

let h(y) = match y with
    | Z -> Z
    | S(y1) -> S(S(h(y1)))


let g(f1,x2,y) = add(f1,y)

let add(x,y) = match x with
    | Z-> y
    | S(x1) -> S(add(x1,y))  