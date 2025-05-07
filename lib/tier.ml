open Faust 

(*****
check for tiering with the function tier_prog
******)

module UF = UnionFind
type ufelem = (string) UF.elem 



(*map fun -> u1..un where ui are the elements in UF*)
type funUFmap = (ufelem list) StringMap.t 
type venv = ufelem StringMap.t

let print_elem e = Printf.printf "%s\n" (UF.get @@ UF.find e) 
let print_classes = List.iter print_elem 

let print_classes_fun f funUFmap = print_string @@"classes of "^f^"\n"; print_classes @@ StringMap.find f funUFmap; print_newline ()

let print_classes funs (funUFmap:funUFmap) = List.iter (fun f -> print_classes_fun f.name funUFmap) funs


(* add all all pairs (x -> t) in venv*)
let add2venv xlist (tlist:ufelem list) (venv:venv) :venv = 
  List.fold_left2 (fun acc x t -> StringMap.add x t acc ) venv xlist tlist

let make_id fname xname = Printf.sprintf "%s_%s" fname xname
let make_id_res fname = Printf.sprintf "%s_res" fname

let merge_name s1 s2 = s1^"/"^s2

let union_list ulist ulist' = 
  List.iter2 (fun u v -> UF.merge merge_name u v |> ignore) ulist ulist'



let new_elem = 
  let cpt= ref 0 in 
  fun () -> incr cpt; UF.make @@ Printf.sprintf "tmp_%d" !cpt

(* fail if the program is not tierable *)
let tier_prog (prog:prog):unit= 
  let create_funUFmap funs = 
    let create_aux (m:funUFmap) (f:fun_def) =
      let tmpvarlst = List.fold_right (fun p acc-> (UF.make @@ make_id f.name p):: acc) f.param [] in 
      let varlst = (UF.make @@ make_id_res f.name):: tmpvarlst in 
      StringMap.add f.name varlst m 
    in
    List.fold_left create_aux StringMap.empty funs
  in
  let funUFmap = 
    let tmp = create_funUFmap prog.fundefs in 
    tmp
  in


  let create_venv (f:fun_def) :venv= 
    add2venv f.param (List.tl @@ StringMap.find f.name funUFmap) StringMap.empty 
  in 

  let rec equiv_expr (fname:string) (venv:venv) expr :ufelem= match expr with
    | Var(x) -> 
      let tmp = StringMap.find x venv in 
      tmp

    | Let(x,e1,e2) ->
      let u = equiv_expr fname venv e1 in 
      let ux = (UF.make (make_id fname x)) in 
      UF.merge merge_name ux u |> ignore;
      equiv_expr fname venv e2

    | Cstr(_,elst) -> 
      if List.is_empty elst then 
        new_elem () 
      else
        let u = equiv_expr fname venv (List.hd elst) in 
        List.iter (fun e -> let u' = equiv_expr fname venv e in ignore @@ UF.merge merge_name  u' u) (List.tl elst);
        u

    | App(f,elist) -> 
      let ulist = StringMap.find f funUFmap in 
      let ulist' = equiv_exprlist fname venv elist in 
      union_list (List.tl ulist) ulist'; (*tl ulist contain the params elem of f*)
      
      List.hd ulist (*hd ulist contain the return elem of f*)

    | Match(e,blist) -> 
      let u = equiv_expr fname venv e in
      let bu = equiv_branch fname venv u (List.hd blist) in 
      List.iter 
        (fun b -> let bu' = equiv_branch fname venv u b in  ignore @@ UF.merge merge_name  bu' bu) 
        (List.tl blist);
      bu

  and equiv_exprlist (fname:string) (venv:venv) (elist:expr list):(ufelem list) = 
    List.fold_right (fun e acc -> equiv_expr fname venv e :: acc) elist [] 

  and equiv_branch (fname:string) (venv:venv) (u:ufelem) (b:type_branch) :ufelem= 
    let ((_,xlist),e) = b in  (*branch of the form _(xlist) -> e*)
    (* create a new element for each x + merge it with u + and add it to venv*)
    let majvenv venv x = 
      let u' = (UF.make (make_id fname x)) in 
      UF.merge merge_name  u' u |> ignore;
      StringMap.add x u' venv 
    in
    let newvenv = List.fold_left majvenv venv xlist in 
    equiv_expr fname newvenv e
  in

  let equiv_fun (f:fun_def) =
    let venv = create_venv f in 
    let u = equiv_expr f.name venv f.body in 
    let ures = List.hd @@ StringMap.find f.name funUFmap in 
    UF.merge merge_name u ures |> ignore 
  in 
  (* first we create the equivalence classes*)
  List.iter (fun f -> equiv_fun f ) prog.fundefs;
  print_classes prog.fundefs funUFmap;

  (* we now create the conditions of tiering (for recursive calls) in a graph *)
  let dep = Syntax.dep_graph prog in 
  let constraints = Syntax.G.create () in 
  let add_constraint f = 
    if Syntax.is_rec f.name dep then
      let elems = StringMap.find f.name funUFmap in 
      Syntax.G.add_edge constraints (UF.get @@ List.hd elems) (UF.get @@ List.hd @@ List.tl elems);
  in
  List.iter add_constraint prog.fundefs ;
  Syntax.print_graph constraints;
  
  (*then we check if there is a cycle in the constraint graph*)
  if Syntax.DFS.has_cycle constraints then
    failwith "tier error: the constraint graph has a loop"