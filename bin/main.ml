open Faustlib.Faust
open Faustlib
open Format
open Lexing
open Parser
let usage = "usage: ./faustml - [itsena] prog.ml"

let tier_flag = ref false 
let type_flag = ref false 
let synt_flag = ref false 
let eval_flag = ref false 
let naive_eval_flag = ref false 
let verbose_flag = ref false

let set_all () =
  tier_flag := true;
  type_flag := true ; 
  synt_flag := true ;
  eval_flag := true;
  naive_eval_flag := true

let set_all_check () = 
  tier_flag := true;
  type_flag := true ; 
  synt_flag := true 

let set_flags (args:string) =
  let set_flag (c:char) = match c with
    | 'i' -> tier_flag := true 
    | 't' -> type_flag := true 
    | 's' -> synt_flag := true 
    | 'e' -> eval_flag := true
    | 'n' -> naive_eval_flag := true
    | 'a' -> set_all ()
    | 'p' -> set_all_check ()
    | _ -> raise (Arg.Bad(Printf.sprintf "%c not recognized" c))
  in
  String.iter (set_flag) args


let spec = [
  ("-", Arg.String set_flags, "condence version of the other options");
  ("-a", Arg.Unit set_all, "enable all");
  ("-p", Arg.Unit set_all_check, "enable polytime check (equiv to -tsi)");
  ("-t", Arg.Set type_flag, "enable type check");
  ("-s", Arg.Set synt_flag, "enable syntax check");
  ("-i", Arg.Set tier_flag, "enable tier check");
  ("-e", Arg.Set eval_flag, "enable standar execution");
  ("-n", Arg.Set naive_eval_flag, "enable naive execution");
  ("-verbose", Arg.Set verbose_flag, "enable verbose for all active options");
]

let file =
    let file = ref None in
    let set_file s =
      if not (Filename.check_suffix s ".ml") then
        raise (Arg.Bad "no .ml extension");
      file := Some s
    in
    Arg.parse spec set_file usage;
    match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let report filename (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" filename l fc lc
    
let parse filename parsefun lb = 
  try 
    parsefun Lexer.token lb  
  with
      | Parser.Error -> report filename (lexeme_start_p lb, lexeme_end_p lb);
    eprintf "syntax error@.";
    exit 1

let () =
  try
    let c_prog  = open_in file in
    let lb_prog = Lexing.from_channel c_prog in
    let prog = parse file Parser.program lb_prog in 
    close_in c_prog;
    if !type_flag then
      Typeinfer.type_inf_prog !verbose_flag prog;
    if !synt_flag then
      Syntax.check_syntax prog;
    if !tier_flag then
      Tierwithdupl.tier_prog !verbose_flag prog; 
    if !type_flag && !tier_flag && !synt_flag then 
      Printf.printf "your code is certified Polytime! (using memoization and sharing)\n";
    if !eval_flag || !naive_eval_flag then (
      let c_input = read_line () in 
      let lb_input = Lexing.from_string c_input in 
      let input =  parse "input" Parser.valuelist lb_input in 
      if !naive_eval_flag then
        Printf.printf "naive exec -> %s\n" @@ Faust.value2string @@ Interpret.eval_prog prog input;
      if !eval_flag then
        Printf.printf "memo+sharing exec -> %s\n" @@ Faust.value2string @@ InterpretMS.eval_prog !verbose_flag prog input);
    exit 0
  with
  | e ->
     eprintf "Anomaly: %s\n@." (Printexc.to_string e);
     exit 2