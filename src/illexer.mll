{
  open Utils
  open Ilparser

  module L = Location
  module S = Ilast

  let unterminated_comment loc =
    raise (S.ParseError (loc, Some "unterminated comment"))

  let invalid_char loc (c : char) =
    let msg = Printf.sprintf "invalid char: `%c'" c in
    raise (S.ParseError (loc, Some msg))

  let _keywords = [
    "w8"    , W8   ;
    "w16"   , W16  ;
    "w32"   , W32  ;
    "w64"   , W64  ;

    "bool"  , BOOL ;
    "int"   , TINT ;

    "else"  , ELSE   ;
    "false" , FALSE  ;
    "goto"  , GOTO   ;
    "if"    , IF     ;
    "label" , LABEL  ;
    "leak"  , LEAK   ;
    "macro" , MACRO  ;
    "signextend", SIGNEXTEND;
    "true"  , TRUE   ;
    "while" , WHILE  ;
    "zeroextend", ZEROEXTEND;
  ]

  let keywords = Hash.of_enum (List.enum _keywords)

  let sign_of_char =
    function
    | 'u' -> S.Unsigned
    | 's' -> S.Signed
    | _ -> assert false

  let mk_sign : char option -> S.sign =
    function
    | Some c -> sign_of_char c
    | None   -> S.Unsigned 

  let mk_shr = function
    | S.Unsigned -> LSR 
    | S.Signed   -> ASR

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
