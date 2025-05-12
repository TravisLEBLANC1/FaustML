open Faust 

(*****
check for syntactic restriction with the function check_syntax
we can also create the depedency graph with dep_graph
******)
module G = Graph.Imperative.Digraph.Concrete(struct
  type t = string
  let compare = compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
module DFS = Graph.Traverse.Dfs(G)
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
  
  let dep_fun (f:fun_def) = dep_expr f.name f.body in 
  List.iter dep_fun prog.fundefs;
  dep

(***************)
(*this is a part of code of the Graph.Traverse.DFS library,
i want the "has_cycle" function but only starting from a particular point*)
let is_in_cycle g start =
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
              try if H.find h w then raise Exit
              with Not_found -> Stack.push w stack)
          g v;
      end
    done
  in
  Stack.push start stack;
  try
    loop ();
    false
  with Exit ->
    true 
(*******************)
let is_rec f (g:G.t) = is_in_cycle g f 
let print_graph (g:G.t) (name:string)= 
  let oc = open_out ("graphs/"^name^".dot") in
  Dot.output_graph oc g;
  (* Printf.printf "graph put in %s.dot\n" name; *)
  close_out oc

(* return true if g depends on f*)
let is_rec_call (dep: G.t) f g = G.mem_edge dep g f


(* return true if e is in the list of variables vars*)
let is_in_var vars e = 
  match e with 
  | Var(z) -> List.mem z vars 
  | _ -> false


let check_syntax (prog:prog) =
  let dep = dep_graph prog in 
  print_graph dep "depedency";

  (*return true if elist == vars element by element*)
  let rec match_vars vars elist = match vars,elist with 
    | [],[] -> true 
    | [],_ -> false 
    | _,[] -> false 
    | z::vars,e::elist -> is_var z e && match_vars vars elist
  in
  let rec check_expr f args safe (e:expr) = match e with 
    | Var(_) -> ()
    | Let(_,e1,e2) -> check_expr f args safe e1; check_expr f args safe e2
    | Cstr(_,elst) -> List.iter (check_expr f args safe) elst 
    | App(h, elst) -> 
      if is_rec_call dep f h then
        if not(match_vars (List.tl args) (List.tl elst)) || not(is_in_var safe (List.hd elst)) then 
          let y = (concat (List.tl args) ",") in 
          failwith @@ Printf.sprintf "the recursif call of "^f^" must have the form "^h^"(x1,"^y^") with x1 a match var of the first argument";
    | Match(e, blst) -> 
      if is_var (List.hd args) e then 
        List.iter (check_branch_safe f args safe) blst  
      else 
        List.iter (check_branch f args safe ) blst 
        
  and check_branch f args safe (b:type_branch) = check_expr f args safe (branch_expr b)
  and check_branch_safe f args safe (b:type_branch) = 
    let xlst = branch_vars b in
    let newsafe =  xlst@safe in 
    check_expr f args newsafe (branch_expr b)
  in  
  let check_fun (f:fun_def) = check_expr f.name f.param [] f.body in 

  List.iter check_fun prog.fundefs;

  Printf.printf "syntaxcheck done\n";


        





  
