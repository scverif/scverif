{
  open Utils
  open Parser 

  module L = Location

  let _keywords = [
    "read", READ;
    "asm",  ASM;
    "il",   IL
    ]

  let keywords = Hash.of_enum (List.enum _keywords)

}

let blank    = [' ' '\t' '\r']
let newline  = ['\n']

let letter = ['0'-'9' 'a'-'z' 'A'-'Z' '/' '\\' '-' '\'' '_' '.' '~']
let ident = letter+ 

rule main = parse 
  | newline { Lexing.new_line lexbuf; main lexbuf }
  | blank+  { main lexbuf }
  | "/*" { comment 0 lexbuf; main lexbuf }

  | "//" [^'\n']* newline { Lexing.new_line lexbuf; main lexbuf }
  | "//" [^'\n']* eof     { main lexbuf }

  | ident+ as s           { odfl (STRING s) (Hash.find_option keywords s) }
  | _ as c  { invalid_char (L.of_lexbuf lexbuf) c }
  | eof                   { EOF }
  
and comment lvl = parse
  | "*/"             { if lvl <= 0 then () else comment (lvl-1) lexbuf }
  | "/*"             { comment (lvl+1) lexbuf }
  | newline          { Lexing.new_line lexbuf; comment lvl lexbuf }
  | [^'\n']          { comment lvl lexbuf }
  | eof              { unterminated_comment (L.of_lexbuf lexbuf) }
