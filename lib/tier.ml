open Faust 

(*****
check for tiering with the function tier_prog
******)

module UF = UnionFind
type ufelem = (string) UF.elem 



(*map fun -> u1..un where ui are the elements in UF*)
type funUFmap = (ufelem list) SMap.t 
type venv = ufelem SMap.t

let print_elem e = Printf.printf "%s\n" (UF.get @@ UF.find e) 
let print_classes = List.iter print_elem 

let print_classes_fun f funUFmap = print_string @@"classes of "^f^"\n"; print_classes @@ SMap.find f funUFmap; print_newline ()

let print_classes funs (funUFmap:funUFmap) = List.iter (fun f -> print_classes_fun f.name funUFmap) funs


(* add all all pairs (x -> t) in venv*)
let add2venv xlist (tlist:ufelem list) (venv:venv) :venv = 
  List.fold_left2 (fun acc x t -> SMap.add x t acc ) venv xlist tlist

let make_tmp_id () = "E"
let make_id fname xname = Printf.sprintf "%s%s" fname xname
let make_id_res fname = Printf.sprintf "%sR" fname

let merge_name s1 s2 = s1^"__"^s2

let merge_classes u1 u2 = UF.merge merge_name u1 u2 |> ignore


let union_list ulist ulist' = 
  List.iter2 (fun u v -> merge_classes u v) ulist ulist' (*TODO un peu random le Some...*)



(* fail if the program is not tierable *)
let tier_prog (verbose:bool) (prog:prog):unit= 
  let create_funUFmap funs = 
    let create_aux (m:funUFmap) (f:fun_def) =
      let tmpvarlst = List.fold_right (fun p acc-> (UF.make @@ make_id f.name p):: acc) f.param [] in 
      let varlst = (UF.make @@ make_id_res f.name):: tmpvarlst in 
      SMap.add f.name varlst m 
    in
    List.fold_left create_aux SMap.empty funs
  in
  let funUFmap = 
    let tmp = create_funUFmap prog.fundefs in 
    tmp
  in


  let create_venv (f:fun_def) :venv= 
    add2venv f.param (List.tl @@ SMap.find f.name funUFmap) SMap.empty 
  in 

  let rec equiv_expr (fname:string) (venv:venv) expr :ufelem= 
    match expr with
    | Var(x) -> (
      try 
        SMap.find x venv
      with 
        Not_found -> failwith @@"variable "^x^ " does not exists" )
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
      let ulist = SMap.find f funUFmap in 
      let ulist' = equiv_exprlist fname venv elist in 
      union_list (List.tl ulist) ulist';   (*tl ulist contain the params elem of f*)
      List.hd ulist                        (*hd ulist contain the return elem of f*)

    | Match(e,blist) -> 
      let u = equiv_expr fname venv e in
      let bu = equiv_branch fname venv u (List.hd blist) in 
      List.iter 
        (fun b -> let bu' = equiv_branch fname venv u b in  ignore @@ merge_classes bu' bu) 
        (List.tl blist);
      bu

    | IfElse(e1,e2,e3) -> 
      equiv_expr fname venv e1 |> ignore;
      let c2 = equiv_expr fname venv e2 in 
      let c3 = equiv_expr fname venv e3 in 
      merge_classes c2 c3;
      c3

  and equiv_exprlist (fname:string) (venv:venv) (elist:expr list) :ufelem list = 
    List.map (fun e -> equiv_expr fname venv e) elist

  and equiv_branch (fname:string) (venv:venv) u (b:type_branch)= 
    let Branch((_,xlist),e) = b in  (*branch of the form _(xlist) -> e*)
    (* create a new element for each x + merge it with u + and add it to venv*)
    let majvenv (venv:venv) x = 
      let u' = UF.make (make_id fname x) in 
      merge_classes  u' u ;
      SMap.add x u' venv 
    in
    let newvenv = List.fold_left majvenv venv xlist in 
    equiv_expr fname newvenv e
  in

  let equiv_fun (f:fun_def) =
    
    
    let venv = create_venv f in 
    let u = equiv_expr f.name venv f.body in 
    let ures = List.hd @@ SMap.find f.name funUFmap in 
    merge_classes u ures;
    print_string @@"--------- " ^ f.name ^ " ------\n";
    print_classes prog.fundefs funUFmap;
  in 
  (* first we create the equivalence classes*)
  List.iter (fun f -> equiv_fun f ) prog.fundefs;
  if verbose then 
    print_classes prog.fundefs funUFmap;

  (* we now create the conditions of tiering (for recursive calls) in a graph *)
  let dep = GraphF.dep_graph prog in 
  let constraints = GraphF.G.create () in 
  let add_constraint f = 
    if GraphF.is_rec dep f.name then
      let elems = SMap.find f.name funUFmap in 
      GraphF.G.add_edge constraints (UF.get @@ List.hd elems) (UF.get @@ List.hd @@ List.tl elems);
  in
  List.iter add_constraint prog.fundefs ;
  if verbose then 
    GraphF.print_graph constraints "constraints";
  
  (*then we check if there is a cycle in the constraint graph*)
  if GraphF.DFS.has_cycle constraints then
    failwith "tier error: the constraint graph has a loop";

  Printf.printf "tiering done\n";