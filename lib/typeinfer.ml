open Faust 
open Typecheck
(*****
check for typing with the function typ_inf_prog 
******)

type gtype = Alpha of string | Base of basetype
type gfuntype = GFun of gtype list*gtype
type gfenv = gfuntype StringMap.t
type gvenv = gtype StringMap.t           (* var => gtyp*)

let fenv2gfenv (fenv:fenv):gfenv = 
  let basetype2gtype b = Base(b) in 
  let funtype2gfuntype f = 
    let Fun(pl,r) = f in 
    GFun(List.map basetype2gtype pl, basetype2gtype r) in 
  StringMap.map funtype2gfuntype fenv 

let add2venv xlist tlist venv = 
  List.fold_left2 (fun acc x t -> StringMap.add x t acc ) venv xlist tlist

let create_venv f ft = let GFun(xlist, _) = ft in add2venv f.param xlist StringMap.empty

let type_inf_prog (prog:prog) = 
  let tenv = fenv2gfenv @@ create_tenv prog.typedefs in 
  let new_var =
    let cpt = ref 0 in
    fun () -> incr cpt; Printf.sprintf "α_%i" !cpt
  in

  (* create new alpha types for each functions *)
  let create_fenv (funs:fun_def list) =
    let new_funtype f = GFun(List.map (fun _ -> Alpha(new_var())) f.param, Alpha(new_var())) in 
    List.fold_left (fun acc f -> StringMap.add f.name (new_funtype f) acc) StringMap.empty funs
  in
  
  (*fenv contains all user type and function (alpha) type*)
  let fenv = StringMap.union (fun _ t1 _ -> Some(t1)) tenv (create_fenv prog.fundefs) in
  
  (* subst contains all substitutions of alpha types found in the program*) 
  let subst = Hashtbl.create 32 in

  let rec unfold t = match t with
    | Base _ -> t
    | Alpha a->
        if Hashtbl.mem subst a then
          unfold (Hashtbl.find subst a)
        else
          t
  in

  let gtype2string gt = 
    match unfold gt with 
      | Base t' -> t' 
      | Alpha t' -> t'
  in
  let print_gfenv (gfenv:gfenv) :unit= 
    let print_mapping c m = 
      let GFun(gtlist,gt) = m in 
      Printf.printf "%s:" c ;
      List.iter (fun t -> Printf.printf "%s " @@ gtype2string t) gtlist ;
      Printf.printf "-> %s\n" @@ gtype2string gt;
    in 
    StringMap.iter print_mapping gfenv 
  in 

  let print_subst = fun () ->  
    let print_subst s = 
      let (a,b) = s in Printf.printf "%s->%s\n" (a) (gtype2string b)
    in
    let slist = Hashtbl.to_seq subst in 
     Seq.iter print_subst slist; 
  in
  let unify t1 t2 = match unfold t1, unfold t2 with
    | Base a, Base b when a=b -> ()
    | Alpha a, Alpha b when a=b -> ()                               
    | Alpha a, t | t, Alpha a -> Hashtbl.add subst a t
    | _, _ -> failwith "unification error"
  in

  let unify_list tlist tlist' = 
    List.iter2 unify tlist tlist' 
  in

  let rec infer_expr e gvenv = match e with 
    | Var(x) -> StringMap.find x gvenv
    
    | Let(x,e1,e2) -> 
      let t = infer_expr e1 gvenv in 
      infer_expr e2 (StringMap.add x t gvenv)

    | Cstr(c, elist) | App(c,elist) -> 
      check_constr fenv c;
      let GFun(tlist, t) = StringMap.find c fenv in
      let tlist' = infer_exprlist elist gvenv in 
      unify_list tlist tlist'; 
      t

    | Match(e, blist) -> 
      let te = infer_expr e gvenv in 
      let (_,tb) = infer_branch (List.hd blist) gvenv in 
      (* check if all branch have the same input and output type *)
      List.iter (fun b -> let (tb1,tb2) = infer_branch b gvenv in unify te tb1; unify tb tb2;) blist;
      tb

  and infer_exprlist (exprl: expr list) (gvenv:gvenv) :(gtype list) =
    List.fold_left (fun acc e -> infer_expr e gvenv :: acc) [] (List.rev exprl)

  and infer_branch b gvenv :(gtype*gtype) = 
    let Branch((c,xlist),e) = b in      (*branch of the form c(xlist) -> e*)
    check_constr fenv c;
    let GFun(tlist, t) = StringMap.find c fenv in (*type c:tlist -> t*)
    let te = infer_expr e (add2venv xlist tlist gvenv) in   (*te=type of e*)
    (t, te)         (* type b:t->te*)
  in 

  let infer_fun (f:fun_def) =
    check_constr fenv f.name;
    let GFun(_,r) as funt =  StringMap.find f.name fenv in 
    let venv = create_venv f funt in 
    let t = infer_expr f.body venv in
    unify t r;
  in 

  List.iter (fun f -> (*Printf.printf "looking at %s \n" f.name;*) infer_fun f;(* print_subst (); print_string "---\n"*)) prog.fundefs;
  print_gfenv fenv;

  Printf.printf "typecheck done\n";
  