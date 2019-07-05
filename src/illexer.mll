{
  open Utils
  open Ilparser
  open Common
  module L = Location
  module S = Ilast

  let _keywords = [
    "w8"    , W8   ;
    "w16"   , W16  ;
    "w32"   , W32  ;
    "w64"   , W64  ;

    "asm"   , ASM;
    "bool"  , BOOL ;
    "int"   , TINT ;
    "uint"  , UINT ;

    "annotation", ANNOTATION;
    "apply" , APPLY  ;
    "else"  , ELSE   ;
    "exit"  , EXIT   ;
    "false" , FALSE  ;
    "goto"  , GOTO   ;
    "include", INCLUDE;
    "if"    , IF     ;
    "il"    , IL     ;
    "init"  , INIT   ;
    "label" , LABEL  ;
    "leak"  , LEAK   ;
    "macro" , MACRO  ;
    "signextend", SIGNEXTEND;
    "region", REGION ;
    "true"  , TRUE   ;
    "verbose", VERBOSE;
    "while" , WHILE  ;
    "zeroextend", ZEROEXTEND;
  ]

  let keywords = Hash.of_enum (List.enum _keywords)

  let sign_of_char =
    function
    | 'u' -> Unsigned
    | 's' -> Signed
    | _ -> assert false

  let mk_sign : char option -> sign =
    function
    | Some c -> sign_of_char c
    | None   -> Unsigned

  let mk_shr = function
    | Unsigned -> LSR
    | Signed   -> ASR

}

(* -------------------------------------------------------------------- *)
let blank    = [' ' '\t' '\r']
let newline  = ['\n']
let digit    = ['0'-'9']
let hexdigit = ['0'-'9' 'a'-'f' 'A'-'F']
let lower    = ['a'-'z']
let upper    = ['A'-'Z']
let letter   = (lower | upper)
let idletter = letter | '_'
let ident    = idletter (idletter | digit)*
let signletter = ['s' 'u']

(* -------------------------------------------------------------------- *)
rule main = parse
  | newline { Lexing.new_line lexbuf; main lexbuf }
  | blank+  { main lexbuf }

  | "/*" { comment 0 lexbuf; main lexbuf }

  | "//" [^'\n']* newline { Lexing.new_line lexbuf; main lexbuf }
  | "//" [^'\n']* eof     { main lexbuf }

  | "\"" { STRING (Buffer.contents (string (Buffer.create 0) lexbuf)) }

  | (digit+) as s
      { INT (Bigint.of_string s) }

  | ("0x" hexdigit+) as s
      { INT (Bigint.of_string s) }

  | ident+ as s
      { odfl (IDENT s) (Hash.find_option keywords s) }

  | "("     { LPAREN     }
  | ")"     { RPAREN     }
  | "["     { LBRACKET   }
  | "]"     { RBRACKET   }
  | "{"     { LCURLY     }
  | "}"     { RCURLY     }
  | "<-"    { LEFTARROW  }
  | ","     { COMMA      }
  | ";"     { SEMICOLON  }
  | "?"     { QUESTIONMARK  }
  | ":"     { COLON  }

  | "<<"                    { LSL                }
  | ">>" (signletter as s)? { mk_shr (mk_sign s) }
  | "<=" (signletter as s)? { LE   (mk_sign s) }
  | "<"  (signletter as s)? { LT   (mk_sign s) }



  | "!"  { NOT      }
  | "+"  { ADD      }
  | "-"  { SUB      }
  | "*"  { MUL      }
  | "**" { MULH     }
  | "&"  { AND      }
  | "^"  { XOR      }
  | "|"  { OR       }
  | "==" { EQ       }
  | "!=" { NEQ      }

  | _ as c  { invalid_char (L.of_lexbuf lexbuf) c }
  | eof     { EOF }

(* -------------------------------------------------------------------- *)
and comment lvl = parse
  | "*/"             { if lvl <= 0 then () else comment (lvl-1) lexbuf }
  | "/*"             { comment (lvl+1) lexbuf }
  | newline          { Lexing.new_line lexbuf; comment lvl lexbuf }
  | [^'\n']          { comment lvl lexbuf }
  | eof              { unterminated_comment (L.of_lexbuf lexbuf) }

and string buf = parse
  | "\""          { buf }
  | newline       { unterminated_string (L.of_lexbuf lexbuf) }
  | blank+        { unterminated_string (L.of_lexbuf lexbuf) }
  | _ as c        { Buffer.add_char buf c   ; string buf lexbuf }
  | eof           { unterminated_string (L.of_lexbuf lexbuf) }
