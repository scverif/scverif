{
  open Utils
  open Asmparser
  open Common

  module L=Location
  exception Error of string

}

let empty = ""
let blank = [' ' '\t' '\r' ]

let newline = '\n'
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let digit   = ['0'-'9']
let char    = upper | lower | digit | '_' | '.' | '-'



rule main = parse
  | newline        { Lexing.new_line lexbuf; main lexbuf}
  | blank+         { main lexbuf }
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
  | ";" [^'\n']*   { main lexbuf }
  | _ as x         { invalid_char (L.of_lexbuf lexbuf) x }
  | eof            { EOF }
