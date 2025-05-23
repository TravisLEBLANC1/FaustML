module SMap = Map.Make(String)
module SSet = Set.Make(String)


(****
contains the type of a faustml program
*****)

type prog = {
  typedefs: type_def list; 
  fundefs: fun_def list;
}
and type_def = string * type_constr list   (*type t = C1 of t1..tn*)
and type_constr = string * string list     (*C(x1,..,xn)*)
and type_branch = Branch of type_constr * expr 
and fun_def = {                            (*f(x1,..,xn) = e*)
  name: string;
  body: expr;
  param: string list; 
}
and expr = 
  | Var of string                               (* x *)
  | Let of string * expr * expr                 (* let x = e1 in e2*)
  | Cstr of string * expr list                  (* C(e1,..,en)*)
  | App of string * expr list                   (* f(e1,..,en)*)
  | Match of expr * (type_branch list)   (*match e with c1->e1..cn->en*)
  | IfElse of expr * expr * expr

type value = VCstr of string * value list (*the value C(v1,..,vn)*)

let (default_booltype:type_def) = ("bool", [("True",[]); ("False", [])])
let branch_expr (b:type_branch) = let Branch((_,_),e)=b in e
let branch_vars (b:type_branch) = let Branch((_,x),_)=b in x

(*return true if e is the variable x*)
let is_var x (e:expr) = match e with
  | Var(z) -> String.equal x z 
  | _ -> false

let rec concat slst del = match slst with 
  | [] -> ""
  | s::slst -> s^del^(concat slst del) 

let rec expr2string = function 
  | Var(x)-> x
  | Let(x,e1,e2)-> "let "^x^" = "^ expr2string e1^" in "^expr2string e2
  | Cstr(c,elst) -> c^"("^(concat (List.map (expr2string) elst) ",")^")"
  | App(f,elst) -> f^"("^(concat (List.map (expr2string) elst) ",")^")"
  | Match(e, blst) -> "match "^ expr2string e ^ " with \n" ^ (concat (List.map (branch2string) blst) "\n")
  | IfElse(e1,e2,e3) -> "if "^expr2string e1 ^ " then " ^ expr2string e2 ^" else" ^ expr2string e3
and branch2string (b:type_branch) = 
  let Branch((c,xlst),e) = b in 
  "| "^c^"("^(concat xlst ",")^") -> "^ expr2string e

let rec value2string (v:value) = 
  let VCstr(c,vlist) = v in 
  if List.is_empty vlist then 
    c
  else
    c^"("^(value2string @@ List.hd vlist)^List.fold_right (fun v acc -> ","^(value2string v)^acc) (List.tl vlist) ")"

let find_fun fname fundefs = 
  List.find (fun g -> String.equal g.name fname) fundefs 

let find_type tname (typedefs:type_def list) = 
  List.find_opt (fun (t,_) -> String.equal t tname) typedefs 

let find_constr cname (typedefs:type_def list) = 
  let find_constr clist = 
   Option.is_some @@ List.find_opt (fun (c,_) -> String.equal c cname) clist 
  in
  List.find_opt (fun (_,clist) -> find_constr clist) typedefs 


let add_association venv xlist vlist = 
  let lx = List.length xlist in 
  let lv = List.length vlist in 
  if lx <> lv then 
    invalid_arg @@ Printf.sprintf "%d!=%d cannot associate xlist with vlist" lx lv 
  else
  List.fold_left2 (fun acc x v -> SMap.add x v acc) venv xlist vlist


let rec find_last l = match l with 
  | [] -> raise Not_found 
  | [x] -> x 
  | _::ls-> find_last ls

let find_main fundefs = 
  try 
    List.find (fun f-> String.equal f.name "main") fundefs
  with 
    Not_found -> failwith "error: missing main function"



let add_default_constr (prog:prog) : prog = 
  let check_default_constr c = 
    if Option.is_some @@ find_constr (fst(c)) prog.typedefs then
      failwith @@fst(c)^ " is a reserved constructor name";
  in
  let check_default_type t = 
    if Option.is_some @@ find_type t prog.typedefs then
      failwith @@"type "^t^ " is a reserved type name";
  in
  let (t,clist) = default_booltype in 
  check_default_type t;
  List.iter check_default_constr clist;
  {typedefs = default_booltype::prog.typedefs; fundefs=prog.fundefs}
