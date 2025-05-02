open Faustlib.Typecheck 
open Faustlib.Faust

let test1 = 
  let types = [
    ("Tree", [("N", []); ("B", ["Tree";"Tree"]) ; ("B2", ["Int"])]);
    ("Int", [("Z", []); ("S", ["Int"])]);
  ] in 
  let (b1:type_branch) = (("Z", []),Cstr("B", [Cstr("N", []);Cstr("N", [])])) in 
  let (b2:type_branch) = (("S", ["y"]), Cstr("B2", [Var "y"])) in
  let (e:expr) = Match(Var("x"), [b1;b2])in 
  let f = {name = "f"; body = e ; param = ["x"]} in 
  let ftype = (["Int"], "Tree") in 
  let (prog:prog) = {typedefs = types; fundefs = [f]} in 
  typ_prog prog (StringMap.singleton "f" ftype)

let () = test1 