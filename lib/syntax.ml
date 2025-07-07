open Faust 
open GraphF

(************
check for syntactic restriction with the function check_syntax 
we can also create the depedency graph with dep_graph
************)


(* return true if e is in the list of variables vars*)
let print_scclist lstlst =
  let print_list lst = 
    print_string "[";
    List.iter (fun e -> print_string @@ e^";" ) lst ;
    print_string "]";
  in 
  print_string "[";
  List.iter (fun l ->  print_list l;  ) lstlst;
  print_string "]"

(* check for linearity of types and fun names
  also return thoses sets (types,constructors,funs)*)
let check_linearity (prog:prog) = 
  let check_types tset t = 
    let (tname,_) = t in 
    if SSet.mem tname tset then 
      failwith @@ "syntax error: multiple def of the type "^tname ;
    SSet.add tname tset
  in 
  let check_constr cset t = 
    let (_,clist) = t in 
    let check_constr_list = fun cset (c,_) -> 
      if SSet.mem c cset then 
        failwith @@ "syntax error: multiple def of the constr "^c ;
      SSet.add c cset
    in 
    List.fold_left check_constr_list cset clist
  in 

  let check_fun fset f = 
    if SSet.mem f.name fset then 
      failwith @@ "syntax error: multiple def of the fun "^f.name ;
    SSet.add f.name fset
  in

  let check_type_names tset t = 
    let (tname,clist) = t in 
    let check_tlist tlist =  
      List.iter (fun t' -> if not @@ SSet.mem t' tset then failwith @@ "syntax error: type "^t'^" unfound in def of "^tname) tlist 
    in
    List.iter (fun (_,tlist) -> check_tlist tlist) clist 
  in
  (* one def by type *)
  let tset = List.fold_left check_types SSet.empty prog.typedefs in 
  (* one def by constr *)
  let cset = List.fold_left check_constr SSet.empty prog.typedefs in
  (* one def by function *)
  let fset = List.fold_left check_fun SSet.empty prog.fundefs in 
  (* all used type exists *)
  List.iter (check_type_names tset) prog.typedefs;

  (tset,cset,fset)

(* fail if f does not satisfy syntactic restrictions*)
let check_fun dep cset fset safe_fset (f:fun_def) =
  (*return true if elst \in vars or p(var)  element by element*)
  let rec match_vars safe elst = match safe,elst with 
    | [],[] -> true 
    | [],_ -> false 
    | _,[] -> false 
    | s::safe,e::elst -> (is_safe_call s safe_fset e) && match_vars safe elst
  in

  (* add z vars to the corresponding set of z (if it exists) *)
  let add_to_safe safe z vars = 
    if String.equal z (List.hd f.param) then 
      (* add vars to the first set*)
      (SSet.union (SSet.of_list vars) (List.hd safe))::List.tl safe
    else
      (* add vars to the set where z appear *)
      let f s = if SSet.mem z s then SSet.union (SSet.of_list vars) s else s in 
      List.map f safe 
  in 

  (*safe contains the template of possible recursive calls 
    that is for each position, a set of possible variables *)
  let rec check_expr safe (e:expr) = match e with 
    | Var(_) -> ()
    | Let(_,e1,e2) -> check_expr safe e1; check_expr safe e2
    | Cstr(c,elst) -> 
      if not @@ SSet.mem c cset then 
        failwith @@ "syntax error: constructor "^c^" not found";
      List.iter (check_expr safe) elst

    | App(h, elst) -> 
      if not @@ SSet.mem h fset then 
        failwith @@ "syntax error: function "^h^" not found in "^f.name;
      (* if List.length elst <> List.length f.param then 
        failwith @@ Printf.sprintf  "number of arguments for a call to `%s` don't match: %d != %d" h (List.length elst) (List.length f.param); *)
      if is_rec_call dep f.name h then(
        if not @@ match_vars safe elst then 
          failwith @@ "the recursif calls of `"^f.name^"()` must respect the syntax form (see README)")
      else 
          List.iter (check_expr safe) elst
    | Match(e, blst) -> 
        List.iter (check_branch safe (getvar e)) blst 
    
    | IfElse(e1,e2,e3) -> 
      check_expr safe e1;
      check_expr safe e2;
      check_expr safe e3
        
  and check_branch safe (var:string option) (b:type_branch)  = 
    let Branch((_,xs),e) = b in 
    if Option.is_some var then 
      let z = Option.get var in
      let newsafe = add_to_safe safe z xs in 
      check_expr newsafe e 
    else
      check_expr safe e
  in

  let init_safe param = 
    (*the first parameter must be stricly smaller, and the other one smaller or equal*)
    SSet.empty :: List.map SSet.singleton (safe_tl param)
  in

  check_expr (init_safe f.param) f.body
  




(* return the set of functions that are so called "safe"
  meaning they return a subterm of their input or an empty constructor 
  and therefore can be used in a parameter substitution 
  (plus they are not recursive to satisfy tiering)*)
let safefunctions dep fundefs = 
  let safefun = ref SSet.empty in 
  let scc_order = List.map (List.map (fun fname -> find_fun fname fundefs)) @@ GraphF.create_scc_order dep in 
  let toporder =  List.concat scc_order in

  let rec issafeexpr = function 
    | Var(_) -> true
    | Let(_,e1,e2) -> issafeexpr e1 && issafeexpr e2
    | Cstr(_,elst) -> List.is_empty elst
    | App(h, elst) -> 
      SSet.mem h !safefun && List.for_all issafeexpr elst
    | Match(e, blst) -> 
      issafeexpr e && List.for_all (fun b -> issafeexpr @@ branch_expr b ) blst
    | IfElse(e1,e2,e3) -> issafeexpr e1 && issafeexpr e2 && issafeexpr e3
  in
  let issafefun (f:fun_def) = 
    if is_rec dep f.name then ()
    else if issafeexpr f.body then 
      safefun := SSet.add f.name !safefun
  in
  List.iter issafefun toporder;
  !safefun

(* fail if one function does not satisfy syntactic restrictions*)
let check_syntax (verbose:bool) (prog:prog) =
  let dep = dep_graph prog in
  let (_,cset,fset) = check_linearity prog in  
  let safe_fset = safefunctions dep prog.fundefs in
  if verbose then 
    print_graph dep "depedency";

  List.iter (check_fun dep cset fset safe_fset) prog.fundefs;
  
  Printf.printf "syntaxcheck done\n"


        





  
