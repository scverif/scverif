(* Copyright 2019-2020 - Inria, NXP *)

{
  open Utils
  open Gasparser
  open Common

  module L = Location
  exception Error of string

  let _keywords = [
    ".align" , DALIGN;
    ".cpu"   , DCPU;
    ".data"  , DDATA;
    ".bss"   , DBSS;
    ".global", DGLOBAL;
    ".syntax", DSYNTAX;
    ".text"  , DTEXT;
    ".thumb" , DTHUMB;
    ".thumb_func" , DTHUMBFUNC;
    ".type"  , DTYPE;
  ]
  let keywords = Hash.of_enum (List.enum _keywords)
}

let blank   = [' ' '\t' '\r']
let newline = ['\n']
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let digit   = ['0'-'9']
let sdigits = ('-'| digit) digit*
let char    = upper | lower | digit | '_' | '.' | '-' | '$'
let hex     = (digit | ['a' - 'f'] | ['A' - 'F'])

rule main = parse
  | newline        { Lexing.new_line lexbuf; main lexbuf }
  | blank+         { main lexbuf }
  | (sdigits) as d { INT (Bigint.of_string d) }
  | (char+) as s   { odfl (IDENT s) (Hash.find_option keywords s) }
  | "["            { LBRACKET }
  | "]"            { RBRACKET }
  | "{"            { LCURLY }
  | "}"            { RCURLY }
  | ","            { COMMA }
  | ":"            { COLON }
  | '#'            { SHARP }
  | '!'            { EXCLAMATION }
  | ";"            { COMMENT (Buffer.contents (linecomment (Buffer.create 0) lexbuf)) }
  | "@"            { COMMENT (Buffer.contents (linecomment (Buffer.create 0) lexbuf)) }
  | "//"           { COMMENT (Buffer.contents (linecomment (Buffer.create 0) lexbuf)) }
  | _ as x         { invalid_char (L.of_lexbuf lexbuf) x }
  | eof            { EOF }

and linecomment buf = parse
  | newline       { Lexing.new_line lexbuf; buf }
  | _ as c        { Buffer.add_char buf c; linecomment buf lexbuf }
  | eof           { unterminated_comment (L.of_lexbuf lexbuf) }
