%{
    open Lexing
    open Faustlib.Faust
%}

%token EOF
%token <string> IDENT
%token <string> CSTR
%token BAR COMMA EQ
%token TYPE OF STAR
%token TRUE FALSE
%token IF THEN ELSE
%token LET REC AND IN
%token MATCH WITH ARROW
%token LPAR RPAR LBRACKET RBRACKET
%token SEMI 

%nonassoc ARROW
%nonassoc BAR
// %nonassoc THEN
// %nonassoc ELSE

%start program valuelist
%type <Faustlib.Faust.prog> program 
%type <Faustlib.Faust.value list> valuelist
%%
    
program:
| typs=list(type_def) funs=list(let_fun_def) EOF { {typedefs = typs; fundefs = funs} }
| typs=list(type_def) f=let_rec_fun_def funs=list(rec_fun_def) EOF  { {typedefs = typs; fundefs = f::funs} }
;

let_rec_fun_def:
| LET REC f=fun_def { f }
;
rec_fun_def:
| AND f=fun_def { f }
;

let_fun_def:
| LET f=fun_def { f }
;

fun_def:
| f=IDENT LPAR params=separated_list(COMMA, IDENT) RPAR EQ e=expression { {name=f; body=e;param=params;} }
;

type_def:
| TYPE id=IDENT EQ cstrs=separated_nonempty_list(BAR, cstr_def) { (id, cstrs) }
;

cstr_def:
| c=CSTR { (c, []) }
| c=CSTR OF targs=separated_nonempty_list(STAR, IDENT) { (c, targs) }
;


expression:
| x=IDENT {Var(x)} 
| f=IDENT LPAR args=separated_list(COMMA, expression) RPAR { App(f, args) }
| c=CSTR { Cstr(c, []) }
| c=CSTR LPAR args=separated_list(COMMA, expression) RPAR { Cstr(c, args) }
| TRUE  { Cstr("True",[]) }
| FALSE { Cstr("False", []) }
| IF e1=expression THEN e2=expression ELSE e3=expression { IfElse(e1,e2,e3)}
| MATCH e=expression WITH branches=branches { Match(e, branches) }
| LET x=IDENT EQ e1=expression IN e2=expression
    { Let(x, e1, e2) }
| LPAR e=expression RPAR { e }
;

branches:
| BAR p=pattern ARROW e=expression { Branch(p, e) :: [] }
| BAR p=pattern ARROW e=expression branches=branches { Branch(p, e) :: branches }
;

pattern:
| c=CSTR { (c, []) }
| c=CSTR LPAR pargs=separated_list(COMMA, IDENT) RPAR { (c, pargs) }
;

value:
| c=CSTR { VCstr(c, []) }
| c=CSTR LPAR args=separated_list(COMMA, value) RPAR { VCstr(c, args) }

valuelist :
| v=value EOF {[v]}
| LBRACKET values=separated_list(SEMI, value) RBRACKET EOF {values}