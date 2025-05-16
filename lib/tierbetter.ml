open Faust 

(*****
check for tiering with the function tier_prog
******)

module UF = UnionFind
type ufelem = (string) UF.elem 
type uflist = ufelem list 
type ufarray = ufelem array
type constraInt = LT of int*int | EQ of int*int
type constraUF = LTUF of ufelem*ufelem


type funUFinfo = {
  orig :ufarray;  (* initial classes of the parameters of f *)
  constr :constraInt list; (* constraints that apply to orig and then to each duplication *)
  dupl: ufarray list; (* duplication classes of the initial parameters of f (respect the same constraints) *)
}
type funUFmap = funUFinfo SMap.t 
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


(*init a funUFmap with recursive constraints, and only the original UFelem for each fun*)
let create_funUFmap funs dep = 
  let create_aux (m:funUFmap) (f:fun_def) =
    let origlist = (UF.make @@ make_id_res f.name ):: (List.map (fun p -> UF.make @@ make_id f.name p) f.param) in  
    let orig = Array.of_list origlist in 
    let constr = if Syntax.is_rec dep f.name then [LT(0,1)] else [] in 
    let dupl = [] in 
    SMap.add f.name {orig;constr;dupl} m 
  in
  List.fold_left create_aux SMap.empty funs

(******
 fail if the program is not tierable 
*****)
let tier_prog (verbose:bool) (prog:prog):unit= 
  let dep = Syntax.dep_graph prog in                        (* graph of syntactic depedencies*)
  let funUFmap = ref @@ create_funUFmap prog.fundefs dep in (* map function to unionfind elems*)
  let (constraints: constraUF list ref)= ref [] in          (* list of raw constaints *)
  let constgraph = Syntax.G.create () in                    (* graph of constraints between UFelems*)

  (* add the constraint that the return value is lower than the first argument *)
  let add_lt_constraint fname ulist = 
    if Syntax.is_rec dep fname then begin  
      constraints := (LTUF(List.hd ulist, List.hd @@ List.tl ulist))::!constraints;
    end
  in

  let add_eq_constraint fname constrlist = 
    let infos = SMap.find fname !funUFmap in 
    let newinfos = {orig=infos.orig; constr=constrlist@infos.constr; dupl=infos.dupl} in 
    funUFmap := SMap.add fname newinfos !funUFmap;
  in

  (* return the ulist of the original argument*)
  let ulistOriginal fname = 
    let infos = SMap.find fname !funUFmap in 
    infos.orig 
  in

  (* look at the resulting classes and deduce equality constraints to put in the funUFmap*)
  let deduce_constrInt fname = 
    let orig = ulistOriginal fname in 
    let n = Array.length orig in 
    let constr = ref [] in 
    for i= 0 to n do 
      for j = i to n do 
        if UF.eq orig.(i) orig.(j) then 
          constr := EQ(i,j) :: !constr 
      done
    done;
    !constr
  in

  (* add all ufelement to the right list in funUFmap (the first one to the first one etc...)*)
  let add2funUFmap fname ulist = 
    let infos = SMap.find fname !funUFmap in 
    let newdupl = ulist ::infos.dupl in 
    funUFmap := SMap.add fname {orig=infos.orig; constr=infos.constr; dupl=newdupl} !funUFmap 
  in
  (* create a new ulist for the function fname. 
  The first element is the one for the return value, the rest are the parameter*)
  let create_ulist fname = 
    let f = find_fun fname prog.fundefs in  
    let tmpvarlst = (UF.make @@ make_id_res f.name):: List.map (fun p -> UF.make @@ make_id f.name p) f.param in 
    let ufarray = Array.of_list @@ tmpvarlst in 
    add2funUFmap fname ufarray;
    add_lt_constraint fname ufarray;
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
    let ures = (ulistOriginal f.name).(0) in 
    merge_classes u ures;
    let constr = deduce_constrInt f.name in 
    add_eq_constraint f.name constr 
    (* print_string @@"--------- " ^ f.name ^ " ------\n";
    print_classes_funs prog.fundefs !funUFmap; *)
  in 
  
  let toporder = List.map (fun fname -> find_fun fname prog.fundefs) @@ List.concat @@ Syntax.create_scc_order dep in 
  List.iter (fun f -> equiv_fun f ) toporder;

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