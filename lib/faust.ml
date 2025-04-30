module StringMap = Map.Make(String)


type prog = {
  type_defs: type_def list; 
  fun_defs: fun_def list;
}
and type_def = string * type_constr list   (*type t = C1 of t1..tn*)
and type_constr = string * string list     (*C(x1,..,xn)*)
and type_branch = type_constr * expr 
and fun_def = {                            (*f(x1,..,xn) = e*)
  name: string;
  body: expr;
  param: string list; 
}
and expr = 
  | Var of string                               (* x *)
  | Let of string * expr * expr                 (* let x = e1 in e2*)
  | Cstr of string * expr list                  (* C(e1,..,en)*)
  | App of string * expr list                   (* f(e1,..,en)*)
  | Match of expr * (type_branch list)   (*match e with c1->e1..cn->en*)

type value = VCstr of string * value list