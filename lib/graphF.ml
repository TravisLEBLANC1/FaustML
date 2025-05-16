open Faust 


module G = Graph.Imperative.Digraph.Concrete(struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
module DFS = Graph.Traverse.Dfs(G)
module Path = Graph.Path.Check(G)
module SCC = Graph.Components.Make(G)
module H = Hashtbl.Make(G.V)
module Dot = Graph.Graphviz.Dot(struct
  include G
  let edge_attributes _ = []
  let default_edge_attributes _ = []
  let get_subgraph _ = None
  let vertex_attributes _ = []
  let vertex_name v = v
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)
module Oper = Graph.Oper.Make(Graph.Builder.I(G))



let print_graph (g:G.t) (name:string)= 
  let oc = open_out ("graphs/"^name^".dot") in
  Dot.output_graph oc g;
  Printf.printf "graph put in %s.dot\n" name;
  close_out oc



let add_allvertex g vlist = List.iter (G.add_vertex g) vlist
let add_alledges g elist = List.iter (G.add_edge_e g) elist 


let dep_graph (prog:prog) :G.t =  
  let dep = G.create ~size:16 () in 
  
  (*add in graph all edged f->g where g appear in e*)
  let rec dep_expr (f:string) (e:expr) = 
    match e with
    | Var(_) -> ()
    | Let(_,e1,e2)-> dep_expr f e1; dep_expr f e2;
    | Cstr(_,elst)-> List.iter (dep_expr f ) elst;
    | App(g, elst) -> G.add_edge dep f g; List.iter (dep_expr f ) elst;
    | Match(e,blst) -> dep_expr f e;  List.iter (dep_branch f) blst
  and dep_branch (f:string) (b:type_branch) = 
    let e = Faust.branch_expr b in 
    dep_expr f e;
  in 
  
  let dep_fun (f:fun_def) = G.add_vertex dep f.name; dep_expr f.name f.body in 
  List.iter dep_fun prog.fundefs;
  dep

(***********)
(* this is a code modified from the Graph.Traverse.Dfs module*)
let is_in_cycle (g:G.t) start =
  let h = H.create 97 in
  let stack = Stack.create () in
  let loop () =
    while not (Stack.is_empty stack) do
      let v = Stack.top stack in
      if H.mem h v then begin
        (* we are now done with node v *)
        (* assert (H.find h v = true); *)
        H.replace h v false;
        ignore (Stack.pop stack)
      end else begin
        (* we start DFS from node v *)
        H.add h v true;
        G.iter_succ
          (fun w ->
            (* modify line ↓ exit only if we found start*)
              try if H.find h w && String.equal w start then raise Exit
              with Not_found -> Stack.push w stack)
          g v;
      end
    done
  in
  try
    Stack.push start stack; loop ();
    false
  with Exit ->
    true
(***********)
let is_rec = is_in_cycle


(* return true if g depends on f*)
let is_rec_call (dep: G.t) f g = let pathchecker = Path.create dep in Path.check_path pathchecker g f 

(* return the list of trongly connected component in the topological order*)
let create_scc_order = SCC.scc_list 

(* make the union of g1 and g2 inside g1*)
let inplace_union g1 g2 = G.iter_edges (fun a b -> G.add_edge g1 a b) g2


let create_scc (g:G.t) = 
  let scc_list = create_scc_order g in
  let res = G.create ~size:(List.length scc_list) () in 
  let sccmap = ref SMap.empty in 
  let aux elist =
    let conc = String.concat "__" elist in
    G.add_vertex res conc;
    List.iter (fun e -> sccmap := SMap.add e conc !sccmap) elist
  in  
  List.iter aux scc_list;

  G.iter_edges (fun e1 e2 -> 
    let scce1,scce2 = SMap.find e1 !sccmap,SMap.find e2 !sccmap in
    if scce1 <> scce2 then 
    G.add_edge res scce1 scce2 ) g; 
  !sccmap,res
