{
  open Utils
  open Asmparser

  module L=Location
  exception Error of string

}

let empty = ""
let blank = [' ' '\t' '\r' ]

let newline = '\n'
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let letter  = upper | lower
let char    = letter | '_'
let digit   = ['0'-'9']
let uint    = digit+
let hex     = ['0'-'9'] | ['a'-'f'] | ['A'-'F']
let hexp    = '0' 'x' hex+
let regident= ('r' digit+ | "lr" | "pc" | "sp" )
let ident   = letter (char | digit)*
let identdot   = ident '.' ident
let immediate = '#' uint

rule main = parse
  | newline        { Lexing.new_line lexbuf; main lexbuf}
  | blank+         { main lexbuf }
  | hexp as h      { HEX h }
  | hex+ as h      { HEX h }
  | regident as r  { REGIDENT r }
  | identdot as id { IDENT id }
  | ident as id    { IDENT id }
  | "<"            { LT }
  | ">"            { GT }
  | "["            { LBRACKET }
  | "]"            { RBRACKET }
  | "{"            { LCURLY }
  | "}"            { RCURLY }
  | ","            { COMMA }
  | "+"            { PLUS }
  | ":"            { COLON }
  | '#' (uint as i) { IMMEDIATE (int_of_string i) }
  | _ as x         { invalid_char (L.of_lexbuf lexbuf) x }
  | eof            { EOF }
