open Faust 

type tier = int                                   (*t*)
type funtier = FunT of tier list * tier          (*t1..tn->k*)
type tiermap = funtier StringMap.t              (* f =>  (t1..tn->k)*)

(* 
let get_tier sigma f = let Faust.FunT(_,t) = sigma f in t


(* tier inference *)

type varref = {
  f:string ; (* function the var come from *)
  n:int ;      (* index in the list of parameter *)
}
type constraints = Eq of varref * varref | Gt of varref * varref 

(* return the list of constraints of the program*)
let get_constraints (prog:prog):constraints list = 
  (* return the list of constraints from the function *)
  let get_constraints (f:fun_def):constraints list = []
    
  in
  List.fold_right (fun f acc -> get_constraints f @ acc ) prog.fun_defs []




(* sigma: function -> tier
   gamma : var -> tier *)
let rec sat_tier_list_const gamma sigma exprl k  = 
if List.for_all (fun e -> Option.is_some @@ sat_tier_expr gamma sigma e k) exprl  then
  Some(k)
else 
  None
and sat_tier_list gamma sigma (exprl:expr list) tierl = 
  match exprl with 
  | [] -> true 
  | e::exprl' -> (
    match tierl with 
    | [] -> false (* should not happen*)
    | t::tierl' -> Option.is_some @@ sat_tier_expr gamma sigma e t && sat_tier_list gamma sigma exprl' tierl' 
  )
and sat_tier_expr gamma sigma expr (k:tier) :tier option = 
  match expr with 
  | Var(x) -> 
    if StringMap.mem x gamma && StringMap.find x gamma == k then 
      Some(k)
    else  
      None
  | Cstr(_,exprl) -> sat_tier_list_const gamma sigma exprl k
  | App(f,exprl) -> (
    let FunT(tierl,k') = sigma f in sat_tier_list gamma sigma exprl tierl
  )
  | Match(e,clist) -> None
  | _ -> None

 *)


(*
let sat_tier (sigma:string -> tier) (prog:prog):bool =
let rec sat_tier_funlist sigma funl = match funl with 
| [] -> true 
| f::funl -> 
  let k = get_tier sigma f.name in 
  let gamma = (create_gamma sigma f) in
  sat_tier_expr gamma sigma f.body k && sat_tier_funlist sigma funl 

sat_tier_funlist sigma prog.fun_defs
*)