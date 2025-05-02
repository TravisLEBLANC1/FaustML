open Faust 

type basetype = string                     (* t *)
type funtype  = basetype list *basetype    (*t1..tn -> t*)
                                            (*T is which types C come from, t1..tn are the types of the arguments*)
type venv = basetype StringMap.t           (* var => typ*)
type fenv = funtype StringMap.t            (* f ==> (t1..tn->t)*)

let print_fenv (fenv:fenv) :unit= 
  let print_mapping c m = 
    let (tlist,t) = m in 
    Printf.printf "%s:" c ;
    List.iter (fun t -> Printf.printf "%s " t) tlist ;
    Printf.printf "-> %s\n" t;
  in 
  StringMap.iter print_mapping fenv 

(* construct the type environement with the type of all constructors*)
let create_tenv typel : fenv = 
  let add_to_env (t:type_def) tenv =
    let (tname,tlist) = t in  
    let add_to_env_constr (t:type_constr) tenv = 
      let (cname,subtypes) = t in  
      StringMap.add cname (subtypes,tname) tenv 
    in
    List.fold_right add_to_env_constr tlist tenv
  in
  List.fold_right add_to_env typel StringMap.empty 

(* check if c is in the fenv *)


let basetype_equal (t1:basetype) (t2:basetype) = String.equal t1 t2

let check_constr fenv c = 
  if not @@ StringMap.mem c fenv then 
    failwith @@ Printf.sprintf "type error: constr %s not found " c

let check_type t1 t2 = 
  if not @@ basetype_equal t1 t2 then 
    failwith @@ Printf.sprintf "type error: %s is not compatible with %s" t1 t2

let check_branch_type t te t1 t2 = 
  if not @@ basetype_equal t t1 then 
    failwith @@ Printf.sprintf "type error: match arg don't have the same type as branch arg %s != %s" t t1;
  if not @@ basetype_equal te t2 then
    failwith @@ Printf.sprintf "type error: all branches don't have the same type: %s != %s" te t2

(* add all all pairs (x -> t) in venv*)
let add2venv xlist tlist venv = 
  List.fold_left2 (fun acc x t -> StringMap.add x t acc ) venv xlist tlist


let create_venv (f:fun_def) (ft:funtype) = add2venv f.param (fst ft) StringMap.empty
  
  

(* fail if the two type list are different,
  cname is the list of the constructor (or function) we come from*)
let rec tlist_match (cname:string) (tlist:basetype list) (tlist':basetype list) =
  match tlist, tlist' with 
    | [], [] -> () 
    | [], _ -> failwith @@ cname ^ " apply to too many arguments" 
    | _, [] -> failwith @@ cname ^ " apply to not enough arguments" 
    | t::tlist, t'::tlist' -> 
      if not @@  basetype_equal t t' then 
        failwith @@ cname^" apply to a wrong arguments," ^t ^" != "^ t' ;
      tlist_match cname tlist tlist'

let typ_prog (p:prog) (fenv:fenv) = 
  let tenv = create_tenv p.typedefs in 
  let fenv = StringMap.union (fun _ t1 _ -> Some(t1)) tenv fenv in 
  print_fenv fenv;

  let rec typ_expr (expr:expr) (venv:venv) :basetype = match expr with 
    | Var(x) -> 
      if not @@ StringMap.mem x venv then 
        failwith @@ "type error: variable"^ x ^"not typed " ;

      StringMap.find x venv 
    | Let(x,e1,e2) -> 
      let t1 = typ_expr e1 venv in 
      typ_expr e2 (StringMap.add x t1 venv)
    | Cstr(c, elist) | App(c,elist) -> 
      check_constr fenv c;
      let (tlist, t) = StringMap.find c fenv in
      let tlist' = typ_exprlist elist venv in 
      tlist_match c tlist tlist'; 
      t
    | Match(e, blist) -> 
      let t1 = typ_expr e venv in 
      let (_,t2) = typ_branch (List.hd blist) venv in 
      (* check if all branch have the same input and output type *)
      List.iter (fun b -> let (t,te) = typ_branch b venv in check_branch_type t te t1 t2;) blist;
      t2
  and typ_exprlist (exprl: expr list) (venv:venv) :(basetype list) =
    List.fold_left (fun acc e -> typ_expr e venv :: acc) [] (List.rev exprl)
  and typ_branch (b:type_branch) (venv:venv):(basetype*basetype)=
    let ((c,xlist),e) = b in      (*branch of the form c(xlist) -> e*)
    check_constr fenv c;
    let (tlist, t) = StringMap.find c fenv in (*type c:tlist -> t*)
    let te = typ_expr e (add2venv xlist tlist venv) in   (*te=type of e*)
    (t, te)         (* type b:t->te*)
  in

  let type_fun (f:fun_def) =
    check_constr fenv f.name;
    let funt =  StringMap.find f.name fenv in 
    let venv = create_venv f funt in 
    typ_expr f.body venv |> ignore;
  in 

  List.iter (fun f -> type_fun f) p.fundefs


