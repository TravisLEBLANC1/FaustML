open Faust 

(*****
check for tiering with the function tier_prog
******)

module UF = UnionFind
type ufelem = (string) UF.elem 
type constra = LT of ufelem*ufelem 

(*
map fun -> (u11..u1k)..(un1..unq) where uij are the elements of the Union Find
each to level list represent a parameter of f i.e
(u11..u1k) is the set of ufelement linked to the first parameter of f
*)
type funUFmap = (ufelem list list) SMap.t 
type venv = ufelem SMap.t

let rec findlast l = match l with 
  | [] -> raise Not_found 
  | [e] -> e 
  | _::l -> findlast l

let print_elem e = Printf.printf "%s; " (UF.get @@ UF.find e) 
let print_classes = List.iter print_elem 
let print_classes_list = List.iter (fun e -> print_classes e; print_newline ())

let print_classes_fun f funUFmap = print_string @@"classes of "^f^"\n"; print_classes_list @@ SMap.find f funUFmap; print_newline ()

let print_classes_funs funs (funUFmap:funUFmap) = List.iter (fun f -> print_classes_fun f.name funUFmap) funs


(* add all all pairs (x -> t) in venv*)
let add2venv xlist (tlist:ufelem list) (venv:venv) :venv = 
  try
    List.fold_left2 (fun acc x t -> SMap.add x t acc ) venv xlist tlist
  with 
    | Invalid_argument(_) -> failwith @@ Printf.sprintf "cannot add to env: xlist=%d != tlist=%d" (List.length xlist) (List.length tlist)

let make_tmp_id () = "E"
let make_id fname xname = Printf.sprintf "%s%s" fname xname
let make_id_res fname = Printf.sprintf "%sR" fname

let merge_name s1 s2 = s1^"_"^s2

let merge_classes u1 u2 = UF.merge merge_name u1 u2 |> ignore


let union_list ulist ulist' = 
  List.iter2 (fun u v -> merge_classes u v) ulist ulist'

let create_funUFmap funs = 
  let create_aux (m:funUFmap) (f:fun_def) =
    let varlst = [UF.make @@ make_id_res f.name] :: List.map (fun p -> [UF.make @@ make_id f.name p]) f.param in  (* associate an empty list for each parameter and the result*) 
    SMap.add f.name varlst m 
  in
  List.fold_left create_aux SMap.empty funs


(* fail if the program is not tierable *)
let tier_prog (verbose:bool) (prog:prog):unit= 
  let funUFmap = ref @@ create_funUFmap prog.fundefs in (* map function to unionfind elems*)
  let dep = Syntax.dep_graph prog in  (* graph of syntactic depedencies*)
  let (constraints: constra list ref)= ref [] in  (* list of raw constaints *)
  let constgraph = Syntax.G.create () in (* graph of constraints between union find elems*)

  (* add the constraint that the return value is lower than the first argument *)
  let add_constraint fname ulist = 
    if Syntax.is_rec dep fname then begin 
      constraints := (LT(List.hd ulist, List.hd @@ List.tl ulist))::!constraints;
    end
  in

  (* return the ulist of the original argument (the last of each list)*)
  let ulistOriginal fname = 
    let ulistlist = SMap.find fname !funUFmap in 
    List.map findlast ulistlist 
  in
  (* add all ufelement to the right list in funUFmap (the first one to the first one etc...)*)
  let add2funUFmap fname ulist = 
    let ulistlist = SMap.find fname !funUFmap in 
    let newulistlist = List.map2 (fun u ul -> u ::ul) ulist ulistlist in 
    funUFmap := SMap.add fname newulistlist !funUFmap 
  in
  (* create a new ulist for the function fname. 
  The first element is the one for the return value, the rest are the parameter*)
  let create_ulist fname = 
    let f = find_fun fname prog.fundefs in  
    let tmpvarlst = List.map (fun p -> UF.make @@ make_id f.name p) f.param in 
    let ulist = (UF.make @@ make_id_res f.name):: tmpvarlst in 
    add2funUFmap fname ulist;
    add_constraint fname ulist;
    ulist
  in
  (* create a mapping of variable to one of the corresponding ufelement of f*)
  let create_venv (f:fun_def) :venv= 
    let ulist = List.tl @@ ulistOriginal f.name in 
    add2venv f.param ulist SMap.empty 
  in 


  let rec equiv_expr (fname:string) (venv:venv) expr :ufelem = 
    match expr with
    | Var(x) -> SMap.find x venv

    | Let(x,e1,e2) ->
      let u = equiv_expr fname venv e1 in 
      let ux = UF.make (make_id fname x) in 
      merge_classes ux u |> ignore;
      let newvenv = SMap.add x ux venv in  
      equiv_expr fname newvenv e2

    | Cstr(_,elst) -> 
      if List.is_empty elst then 
        UF.make @@ make_tmp_id ()
      else
        let u = equiv_expr fname venv (List.hd elst) in 
        List.iter (fun e -> let u' = equiv_expr fname venv e in ignore @@ merge_classes  u' u) (List.tl elst);
        u

    | App(f,elist) -> 
      let ulist = if Syntax.is_rec_call dep fname f then ulistOriginal f else create_ulist f in
      let ulist' = equiv_exprlist fname venv elist in 
      union_list (List.tl ulist) ulist';        (*tl ulist contain the params elem of f*)
      List.hd ulist                        (*hd ulist contain the return elem of f*)

    | Match(e,blist) -> 
      let u = equiv_expr fname venv e in
      let bu = equiv_branch fname venv u (List.hd blist) in 
      List.iter 
        (fun b -> let bu' = equiv_branch fname venv u b in  ignore @@ merge_classes bu' bu) 
        (List.tl blist);
      bu

  and equiv_exprlist (fname:string) (venv:venv) (elist:expr list) = 
    List.fold_right (fun e acc -> equiv_expr fname venv e :: acc) elist [] 

  and equiv_branch (fname:string) (venv:venv) u (b:type_branch)= 
    let Branch((_,xlist),e) = b in  (*branch of the form _(xlist) -> e*)
    (* create a new element for each x + merge it with u + and add it to venv*)
    let majvenv (venv:venv) x = 
      let u' = UF.make (make_id fname x)in 
      merge_classes  u' u ;
      SMap.add x u' venv 

    in
    let newvenv = List.fold_left majvenv venv xlist in 
    equiv_expr fname newvenv e
  in

  let equiv_fun (f:fun_def) =
    let venv = create_venv f in 
    let u = equiv_expr f.name venv f.body in 
    let ures = List.hd @@ ulistOriginal f.name in 
    merge_classes u ures;
    print_string @@"--------- " ^ f.name ^ " ------\n";
    print_classes_funs prog.fundefs !funUFmap;
  in 
  (* first we create the equivalence classes*)
  List.iter (fun f -> equiv_fun f ) prog.fundefs;
  if verbose then 
    print_classes_funs prog.fundefs !funUFmap;

  let add_constraint f = ()
  in
  (* we now create the conditions of tiering (for recursive calls) in a graph *)
  List.iter add_constraint prog.fundefs ;
  if verbose then 
    Syntax.print_graph constgraph "constraints";
  
  (*then we check if there is a cycle in the constraint graph*)
  if Syntax.DFS.has_cycle constgraph then
    failwith "tier error: the constraint graph has a loop";

  Printf.printf "tiering done\n";