(* Copyright 2014-2020 - Inria *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)
(* -------------------------------------------------------------------- *)
{
open MLTParser
open MLTAst

let unterminated_comment loc =
  raise (ParseError (loc, Some "unterminated comment"))

let illegal_character loc c =
  raise (ParseError (loc, Some (Printf.sprintf "illegal character: %c" c)))

let keywords = Hashtbl.create 97

let () =
  Hashtbl.add keywords "bint8_t"   BINT8_T ;
  Hashtbl.add keywords "bint16_t"  BINT16_T;
  Hashtbl.add keywords "bint32_t"  BINT32_T;
  Hashtbl.add keywords "bint64_t"  BINT64_T;
  Hashtbl.add keywords "uint8_t"   UINT8_T ;
  Hashtbl.add keywords "uint16_t"  UINT16_T;
  Hashtbl.add keywords "uint32_t"  UINT32_T;
  Hashtbl.add keywords "uint64_t"  UINT64_T;
  Hashtbl.add keywords "int8_t"    INT8_T  ;
  Hashtbl.add keywords "int16_t"   INT16_T ;
  Hashtbl.add keywords "int32_t"   INT32_T ;
  Hashtbl.add keywords "int64_t"   INT64_T ;
  Hashtbl.add keywords "int"       INT_T   ;
  Hashtbl.add keywords "void"      VOID    ;
  Hashtbl.add keywords "for"       FOR     ;
  Hashtbl.add keywords "if"        IF      ;
  Hashtbl.add keywords "else"      ELSE    ;
  Hashtbl.add keywords "return"    RETURN
}

(* -------------------------------------------------------------------- *)
let blank    = [' ' '\t' '\r']
let newline  = '\n'
let upper    = ['A'-'Z']
let lower    = ['a'-'z']
let letter   = upper | lower
let digit    = ['0'-'9']
let hexdigit = ['0'-'9''A'-'F''a'-'f']
let uint     = digit+
let uhex     = "0x" hexdigit+
let ident    = (letter | '_') (letter | digit | '_')*
let comment1 = [^'\n']* newline?

(* -------------------------------------------------------------------- *)
rule main = parse
| newline       { Lexing.new_line lexbuf; main lexbuf }
| blank+        { main lexbuf }
| uint  as x    { INT (Z.of_string x) }
| uhex  as x    { INT (Z.of_string x) }
| ident as id   { try Hashtbl.find keywords id with Not_found -> IDENT (mksymbol id) }
| "++"          { PLUSPLUS }
| "--"          { MINUSMINUS }
| "("           { LPAREN }
| ")"           { RPAREN }
| "{"           { LBRACE }
| "}"           { RBRACE }
| "["           { LBRACKET }
| "]"           { RBRACKET }
| ","           { COMMA  }
| "="           { EQUAL  }
| "=="          { EQEQ   }
| "<="          { LEQ }
| ">="          { GEQ }
| "<"           { LANGLE }
| ">"           { RANGLE }
| "<<"          { LLANGLE }
| ">>"          { RRANGLE }
| ";"           { SEMICOLON }
| "+"           { PLUS }
| "*"           { MUL }
| "/"           { DIV }
| "-"           { MINUS }
| "%"           { MOD }
| "~"           { TILDE }
| "&"           { AMP }
| "^"           { HAT }
| "|"           { BAR }
| "+="          { PLUSEQ }
| "-="          { MINUSEQ }
| "&="          { AMPEQ }
| "^="          { HATEQ }
| "|="          { BAREQ }
| "<<="         { LLANGEQ }
| ">>="         { RRANGEQ }
| "/*"          { comment lexbuf; main lexbuf }
| "//" comment1 { Lexing.new_line lexbuf; main lexbuf }
| eof           { EOF }
|  _ as c       { illegal_character (Location.of_lexbuf lexbuf) c }

and comment = parse
| "*/"        { () }
| newline     { Lexing.new_line lexbuf; comment lexbuf }
| eof         { unterminated_comment (Location.of_lexbuf lexbuf) }
| _           { comment lexbuf }
