{
    open Lexing
    open Parser

    let keyword_or_ident =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "let",   LET;
        "in",    IN;
        "type",  TYPE;
        "of",    OF;
        "match", MATCH;
        "with",  WITH;
      ] ;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
}
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*
let cstr = ['A'-'Z'] alpha*
  

rule token = parse
  | ['\n']
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*" 
      { comment lexbuf; token lexbuf }
  | ident as id
      { keyword_or_ident id }
  | cstr as c
      { CSTR c }
  | "=" 
      { EQ }
  | "->"
      { ARROW }
  | "("
      { LPAR }
  | ")"
      { RPAR }
  | ","
      { COMMA }
  | "|"
      { BAR }
  | "*"
      { STAR }
  | _
      { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
  | eof
      { EOF }

and comment = parse
  | "*)"
      { () }
  | "(*"
      { comment lexbuf; comment lexbuf }
  | _
      { comment lexbuf }
  | eof
      { failwith "unfinished comment" }