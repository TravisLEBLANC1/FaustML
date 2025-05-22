open Faust 

(*
naive interpretor call by value, do not use neither memoization nor sharing
*)

type venv = value SMap.t

let is_branch v b = 
  let Branch((q,_), _) =b  in 
  let VCstr(c,_)=v in String.equal q c

let rec eval (venv:venv) (fundefs:fun_def list) = function 
  | Var(x) -> SMap.find x venv 
  | Let(x,e1,e2) -> 
    let v = eval venv fundefs e1 in 
    eval (SMap.add x v venv) fundefs e2

  | Cstr(c, elist) -> VCstr(c, eval_elist venv fundefs elist)

  | App(fname, elist) -> 
    let f = find_fun fname fundefs in
    let vlist = eval_elist venv fundefs elist in 
    
    let newvenv =  try add_association SMap.empty f.param vlist  
      with Invalid_argument(_) -> 
        invalid_arg @@ Printf.sprintf "wrong number of argument as input %s takes %d" f.name (List.length f.param)
      in
    eval newvenv fundefs f.body

  | Match(e, blist) -> 
    let v = eval venv fundefs e in 
    eval_blist venv fundefs v blist 

  | IfElse(e1,e2,e3) -> 
    let v = eval venv fundefs e1 in 
    let VCstr(b,_) = v in 
    if String.equal b "True" then 
      eval venv fundefs e2 
    else if String.equal b "False" then
      eval venv fundefs e3 
    else 
      failwith @@ "runtime error: bad ifelse arg "^b^" is not a boolean value"

and eval_elist venv fundefs elist = List.map (eval venv fundefs) elist

and eval_blist venv fundefs v blist = 
  let b = List.find (is_branch v) blist in 
  eval_branch venv fundefs v b 

and eval_branch venv fundefs v b =    
  let Branch((_,xlist),e) = b in 
  let VCstr(_, vlist) = v in 
  let newvenv =  add_association venv xlist vlist in 
  eval newvenv fundefs e 

let eval_prog prog vlist = 
  let f = find_main prog.fundefs in
  let venv =  try add_association SMap.empty f.param vlist  
    with Invalid_argument(_) -> 
      invalid_arg @@ Printf.sprintf "wrong number of argument as input %s takes %d" f.name (List.length f.param)
    in
  eval venv prog.fundefs f.body


