open Faust 

(*
interpretor call by value with memoization and sharing
the value is unfold only at the end
*)

type loc = int 
type heapval = HCstr of string*loc list (* C(l1,..,ln)*)
type heap = heapval array  (*[C;Q;C(1,1);D(2);...]*)

type fcall = FC of string * loc list (* the function call f(v1..vn)*)
type cache = (fcall, loc) Hashtbl.t


type venv = loc SMap.t


let rec unfold heap l :value =
  let HCstr(c,loclist) = heap.(l) in 
  VCstr(c,List.map (unfold heap) loclist)

let print_array printelem endline size arr = 
  if size > 0 then
    printelem arr.(0);
    for i = 1 to size -1 do 
      print_string endline;
      printelem arr.(i);
    done 
  
let print_list printelem endline lst = 
  if not @@ List.is_empty lst then (
    printelem @@ List.hd lst;
    List.iter (fun e -> print_string endline; printelem e) (List.tl lst) )

let print_heap heap hsize =
  let print_hval hv = 
    let HCstr(c,loclist) = hv in 
    Printf.printf "%s(" c ;
    print_list (Printf.printf "%d") "," loclist;
    Printf.printf ")";
  in 
  print_array print_hval "; " hsize heap 

let eval_prog verbose prog vlist = 
  let cache:cache = Hashtbl.create 2048 in 
  let heap :heap = Array.make 2048 @@ HCstr("_EMPTY", []) in 
  let hsize = ref 0 in 
  let fundefs = prog.fundefs in 

  (* merge the heap with a heapval (add if needed or else read)
  keep the heap maximally shared*)
  let merge heapval = 
    let l = Array.find_index ((=) heapval) heap in   (*       TODO : how to find index????       *)
    if Option.is_some l then 
      Option.get l               (*read the heap*)
    else(
      heap.(!hsize) <- heapval;  (* add to the heap*)
      let l = !hsize in 
      incr hsize;
      if verbose then(
        print_heap heap !hsize;
        print_string "\n-----\n");
      l)
  in 

  (* init the heap to contains vlist, and return loclist corresponding to it*)
  let init_heap vlist =  
    let rec init_value v =
      let VCstr(c,vlist) = v in 
      merge (HCstr(c,init_vlist vlist))
    and init_vlist vlist = 
      List.map (init_value) vlist 
    in 
    init_vlist vlist 
  in

  (*return true if b is the branch of l*)
  let is_branch l b = 
    let Branch((q,_), _) =b  in 
    let HCstr(c,_) = heap.(l) in String.equal q c
  in 


  let rec eval (venv:venv) (expr:expr):loc = 

    match expr with  
    | Var(x) -> SMap.find x venv 
    | Let(x,e1,e2) -> 
      let l = eval venv e1 in 
      eval (SMap.add x l venv) e2

    | Cstr(c, elist) -> 
      let loclist = eval_elist venv  elist in 
      merge @@ HCstr(c,loclist)   (* add or read the heap*)

    | App(fname, elist) -> 
      let f = find_fun fname fundefs in
      let loclist = eval_elist venv  elist in 
      let fc = FC(fname,loclist) in 
      if Hashtbl.mem cache fc then 
        Hashtbl.find cache fc        (* read the cache *)
      else
        let newvenv =  try add_association SMap.empty f.param loclist  
          with Invalid_argument(_) -> 
            invalid_arg @@ Printf.sprintf "wrong number of argument as input %s takes %d" f.name (List.length f.param)
        in
        let res = eval newvenv f.body in 
        Hashtbl.add cache fc res; (* add to the cache f(l1..ln)->l*)
        res

    | Match(e, blist) -> 
      let l = eval venv e in 
      eval_blist venv l blist 

  and eval_elist venv elist = List.map (eval venv) elist

  and eval_blist venv l blist =
    let b = try  
      List.find (is_branch l) blist
    with
      | Not_found -> failwith @@ Printf.sprintf "no match found in the branches of %d" l 
    in
    eval_branch venv l b 

  and eval_branch venv  l b =    
    let Branch((_,xlist),e) = b in 
    let HCstr(_, vlist) = heap.(l) in 
    let newvenv = add_association venv xlist vlist in 
    eval newvenv e 
  in
  let f = List.hd fundefs in
  let loclist = init_heap vlist in 
  let venv =  try add_association SMap.empty f.param loclist  
  with Invalid_argument(_) -> 
    invalid_arg @@ Printf.sprintf "wrong number of argument as input %s takes %d" f.name (List.length f.param)
  in
  
  unfold heap @@ eval venv f.body


