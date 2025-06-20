open Faust 


(************
check for tiering with the function tier_prog
this tiering allow multiple tier for each function and downward coercion
************)

type id = string

type funC = {           (* function constrains*)
  res : id;          
  param : id list; 
  ltgraph :GraphF.G.t;     (* < constraints *)
  leqgraph: GraphF.G.t;    (* <= constraints *)
}
type funCMap = funC SMap.t   (*function constrains map*)
type venv = id SMap.t

let cpt_id = ref 0 
let cpt_copy = ref 0
let make_tmp_id () = incr cpt_id; Printf.sprintf "E%d" !cpt_id
let make_id fname xname = incr cpt_id; Printf.sprintf "%s%s" fname xname
let make_id_res fname = Printf.sprintf "%sR" fname

let create_funUFmap funs dep = 
  let add_fconst ltgraph leqgraph m f = 
    let res = (make_id_res f.name ) in 
    let param = (List.map (fun p -> make_id f.name p) f.param) in

    if GraphF.is_rec dep f.name then 
      GraphF.G.add_edge ltgraph res (List.hd param) ;
    SMap.add f.name {res;param;ltgraph;leqgraph} m 
  in
  let create_aux m flist =
    (* the same graph for all recursive functions*)
    let ltgraph = GraphF.G.create () in 
    let leqgraph = GraphF.G.create () in 
    List.fold_left (add_fconst ltgraph leqgraph) m flist
  in
  List.fold_left create_aux SMap.empty funs

let majvenv fname (venv:venv) x = 
  let u' = make_id fname x in 
  SMap.add x u' venv 

let constrains_fun dep verbose funCMap (f:fun_def) = 
  let fconst = SMap.find f.name funCMap in

  let isleq a b = 
    (* Printf.printf "%s <= %s\n" a b; *)
    GraphF.G.add_edge fconst.leqgraph a b
  in  
  let fun_union fconst gconst clist =  
    GraphF.inplace_union fconst.ltgraph gconst.ltgraph;
    GraphF.inplace_union fconst.leqgraph gconst.leqgraph;
    List.iter2 isleq gconst.param clist
  in
  let getgconst =
    fun g ->  
    let gconst = SMap.find g funCMap in 
    if GraphF.is_rec_call dep f.name g then begin
      gconst 
    end
    else begin
      incr cpt_copy;
      (* Printf.printf "%s-> %s copy%d\n" f.name g !cpt_copy; *)
      let map_id = fun x -> Printf.sprintf "%scp%d" x !cpt_copy in 
      let leqgraph = GraphF.G.map_vertex map_id @@ GraphF.G.copy gconst.leqgraph in 
      let ltgraph = GraphF.G.map_vertex map_id @@ GraphF.G.copy gconst.ltgraph in 
      let param = List.map map_id gconst.param in 
      let res = map_id gconst.res in 
      {res;param;ltgraph;leqgraph}
    end
  in 


  let rec constrains_expr venv expr = 
    match expr with
    | Var(x) -> SMap.find x venv 

    | Let(x,e1,e2) -> 
      let c = constrains_expr venv e1  in 
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
      let clist = List.map (constrains_expr venv) elist in 
      let gconst = getgconst g in 
      fun_union fconst gconst clist ;
      gconst.res
    
    | IfElse(e1,e2,e3) -> 
      let tmpid = make_tmp_id () in 
      constrains_expr venv e1 |> ignore;
      let c2 = constrains_expr venv e2 in 
      let c3 = constrains_expr venv e3 in 
      isleq tmpid c2; 
      isleq tmpid c3; 
      tmpid


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

  if verbose then (
    GraphF.print_graph fconst.leqgraph @@ f.name^"_leq";
    GraphF.print_graph fconst.ltgraph @@ f.name^"_lt")


let testfunconst verbose fname fconst =
  let sccmap,sccgraph = GraphF.create_scc fconst.leqgraph in 
  if verbose then 
    GraphF.print_graph sccgraph @@ fname^"_pre_sccgraph";
  
  GraphF.G.iter_edges (fun e1 e2 -> 
    let scce1,scce2 = SMap.find e1 sccmap,SMap.find e2 sccmap in 
    GraphF.G.add_edge sccgraph scce1 scce2) 
    fconst.ltgraph ;
  if verbose then 
    GraphF.print_graph sccgraph @@ fname^"_sccgraph";
  not @@ GraphF.DFS.has_cycle sccgraph

let tier_prog (verbose:bool) (prog:prog):unit= 
  let dep = GraphF.dep_graph prog in                        (* graph of syntactic depedencies*)
  let scc_order = List.map (List.map (fun fname -> find_fun fname prog.fundefs)) @@ GraphF.create_scc_order dep in 
  let funCMap = create_funUFmap scc_order dep in (* map function to unionfind elems*)

  let toporder =  List.concat scc_order in
  List.iter (constrains_fun dep verbose funCMap) toporder;

  if not @@ List.for_all (fun f -> testfunconst verbose f.name (SMap.find f.name funCMap)) prog.fundefs then 
    failwith "the scc graph has a cycle-> not tierable";
  
  Printf.printf "tiering done \n";
  ()