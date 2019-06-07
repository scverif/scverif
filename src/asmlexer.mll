{
  open Asmparser

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
let regident= 'r' digit+
let ident   = letter (char | digit)*
let immediate = ('#' hex+)

rule main = parse
  | newline        { Lexing.new_line lexbuf; main lexbuf}
  | blank+         { main lexbuf }
  | hex+ as h      { HEX h }
  | regident as r  { REGIDENT r }
  | ident as id    { IDENT id }
  | "<"            { LT }
  | ">"            { GT }
  | "["            { LBRACKET }
  | "]"            { RBRACKET }
  | "{"            { LCURLY }
  | "}"            { RCURLY }
  | ","            { COMMA }
  | ":"            { COLON }
  | "eof"          { EOF }
  | "\n"           { EOL }
  | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
