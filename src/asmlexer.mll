(* Copyright 2019 - Inria, NXP *)

{
  open Utils
  open Asmparser
  open Common

  module L=Location
  exception Error of string

}

let blank = [' ' '\t' '\r']

let newline = '\n'
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let digit   = ['0'-'9']
let char    = upper | lower | digit | '_' | '.' | '-'
let hex     = (digit | ['a' - 'f'])
let fourhex = ('0' 'x')? hex hex hex hex
let hexseq  = fourhex (' ' hex hex hex hex)*

rule main = parse
  | newline        { Lexing.new_line lexbuf; EOL}
  | blank+         { main lexbuf }
  | hexseq as h    { HEX h }
  | fourhex+ as h  { HEX h }
  | char* as id    { IDENT id }
  | "<"            { LT }
  | ">"            { GT }
  | "["            { LBRACKET }
  | "]"            { RBRACKET }
  | "{"            { LCURLY }
  | "}"            { RCURLY }
  | ","            { COMMA }
  | "+"            { PLUS }
  | ":"            { COLON }
  | '#'            { SHARP }
  | '!'            { EXCLAMATION }
  | ";"            { COMMENT (Buffer.contents (linecomment (Buffer.create 0) lexbuf)) }
  | "//" [^'\n']*  { main lexbuf }
  | "/*"           { comment 0 lexbuf; main lexbuf }
  | _ as x         { invalid_char (L.of_lexbuf lexbuf) x }
  | eof            { EOF }

and comment lvl = parse
  | "*/"          { if lvl <= 0 then () else comment (lvl-1) lexbuf }
  | "/*"          { comment (lvl+1) lexbuf }
  | newline       { Lexing.new_line lexbuf; comment lvl lexbuf }
  | [^'\n']       { comment lvl lexbuf }
  | eof           { unterminated_comment (L.of_lexbuf lexbuf) }

and linecomment buf = parse
  | newline       { Lexing.new_line lexbuf; buf }
  | '('           { linecomment buf lexbuf }
  | ')'           { linecomment buf lexbuf }
  | blank+        { linecomment buf lexbuf }
  | _ as c        { Buffer.add_char buf c; linecomment buf lexbuf }
  | eof           { unterminated_comment (L.of_lexbuf lexbuf) }
