open Faust 

type id = string

type funC = {    (* function constrains*)
  res : id;          
  param : id list; 
  ltgraph :GraphF.G.t;     (* < constraints *)
  leqgraph: GraphF.G.t;    (* <= constraints *)
}
type funCMap = funC SMap.t   (*function constrains map*)
type venv = id SMap.t

let cpt = ref 0 
let make_tmp_id () = incr cpt; Printf.sprintf "E%d" !cpt
let make_id fname xname = incr cpt; Printf.sprintf "%s%s_%d" fname xname !cpt
let make_id_res fname = Printf.sprintf "%sR" fname

let create_funUFmap funs dep = 
  let create_aux (m:funCMap) (f:fun_def) =
    let res = (make_id_res f.name ) in 
    let param = (List.map (fun p -> make_id f.name p) f.param) in
    let ltgraph = GraphF.G.create () in 
    let leqgraph = GraphF.G.create () in 
    if GraphF.is_rec dep f.name then 
      GraphF.G.add_edge ltgraph res (List.hd param) ;
    SMap.add f.name {res;param;ltgraph;leqgraph} m 
  in
  List.fold_left create_aux SMap.empty funs

let majvenv fname (venv:venv) x = 
  let u' = make_id fname x in 
  SMap.add x u' venv 

let constrains_fun verbose funCMap (f:fun_def) = 
  let fconst = SMap.find f.name funCMap in

  let isleq a b = 
    Printf.printf "%s <= %s\n" a b;
    GraphF.G.add_edge fconst.leqgraph a b
  in  
  let fun_union fconst gconst clist =  
    GraphF.inplace_union fconst.ltgraph gconst.ltgraph;
    GraphF.inplace_union fconst.leqgraph gconst.leqgraph;
    List.iter2 isleq gconst.param clist
    (* not trivially true for recursive calls
    but it should work because g will call f and we will have xf <= xg and xg <= xf for each x
    plus we do want the union of constraints in both way*)
  in

  let rec constrains_expr venv expr = 
    match expr with
    | Var(x) -> SMap.find x venv 

    | Let(x,e1,e2) -> 
      let c = constrains_expr venv e1  in 
      isleq x c; 
      constrains_expr (SMap.add x c venv) e2 
    
    | Cstr(_,elist) -> 
      let tmpid = make_tmp_id () in 
      List.iter (fun e -> let c = constrains_expr venv e in isleq tmpid c) elist;
      tmpid 
    
    | Match(e,blist) -> 
      let c = constrains_expr venv e in 
      let tmpid = make_tmp_id () in 
      List.iter (constrains_branch c tmpid venv) blist;
      tmpid 

    | App(g,elist) -> 
      let gconst = SMap.find g funCMap in 
      let clist = List.map (constrains_expr venv) elist in 
      fun_union fconst gconst clist;
      gconst.res

  and constrains_branch c tmpid venv b = 
    let Branch((_,xlist),e) = b in 
    
    let newvenv = List.fold_left (majvenv f.name) venv xlist in 
    List.iter (fun x -> isleq (SMap.find x newvenv) c) xlist;
    let c' = constrains_expr newvenv e in 
    isleq tmpid c'
  in
  
  let venv = List.fold_left2 (fun venv p s -> SMap.add p s venv) SMap.empty f.param fconst.param in 
  let idbody = constrains_expr venv f.body in 
  isleq fconst.res idbody;

  if verbose then 
    GraphF.print_graph fconst.leqgraph @@ f.name^"_leq";
    GraphF.print_graph fconst.ltgraph @@ f.name^"_lt"


let testfunconst fname fconst =
  let sccmap,sccgraph = GraphF.create_scc fconst.leqgraph in 
  GraphF.print_graph sccgraph @@ fname^"_pre_sccgraph";
  GraphF.G.iter_edges (fun e1 e2 -> 
    let scce1,scce2 = SMap.find e1 sccmap,SMap.find e2 sccmap in 
    GraphF.G.add_edge sccgraph scce1 scce2) 
    fconst.ltgraph ;

  GraphF.print_graph sccgraph @@ fname^"_sccgraph";
  not @@ GraphF.DFS.has_cycle sccgraph

let tier_prog (verbose:bool) (prog:prog):unit= 
  let dep = GraphF.dep_graph prog in                        (* graph of syntactic depedencies*)
  let funCMap = create_funUFmap prog.fundefs dep in (* map function to unionfind elems*)
  
  let toporder = List.map (fun fname -> find_fun fname prog.fundefs) @@ List.concat @@ GraphF.create_scc_order dep in 
  List.iter (constrains_fun verbose funCMap) toporder;

  if not @@ List.for_all (fun f -> testfunconst f.name (SMap.find f.name funCMap)) prog.fundefs then 
    failwith "the scc graph has a cycle-> not tierable";
  
  ()