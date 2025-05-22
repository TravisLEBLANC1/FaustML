open Faust 
open GraphF
(*****
check for syntactic restriction with the function check_syntax
we can also create the depedency graph with dep_graph
******)


(* return true if e is in the list of variables vars*)
let is_in_var vars e = 
  match e with 
  | Var(z) -> List.mem z vars 
  | _ -> false

let print_scclist lstlst =
  let print_list lst = 
    print_string "[";
    List.iter (fun e -> print_string @@ e^";" ) lst ;
    print_string "]";
  in 
  print_string "[";
  List.iter (fun l ->  print_list l;  ) lstlst;
  print_string "]"



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
      if is_rec_call dep f h then(
        if not(match_vars (List.tl args) (List.tl elst)) || not(is_in_var safe (List.hd elst)) then 
          let y = (concat (List.tl args) ",") in 
          failwith @@ Printf.sprintf "the recursif call of "^f^" must have the form "^h^"(x1,"^y^") with x1 a match var of the first argument";)
      else 
          List.iter (check_expr f args safe) elst
    | Match(e, blst) -> 
      if is_var (List.hd args) e then 
        List.iter (check_branch_safe f args safe) blst  
      else 
        List.iter (check_branch f args safe ) blst 
    
    | IfElse(e1,e2,e3) -> 
      check_expr f args safe e1;
      check_expr f args safe e2;
      check_expr f args safe e3
        
  and check_branch f args safe (b:type_branch) = check_expr f args safe (branch_expr b)
  and check_branch_safe f args safe (b:type_branch) = 
    let xlst = branch_vars b in
    let newsafe =  xlst@safe in 
    check_expr f args newsafe (branch_expr b)
  in  
  let check_fun (f:fun_def) = check_expr f.name f.param [] f.body in 

  List.iter check_fun prog.fundefs;

  Printf.printf "syntaxcheck done\n";


        





  
