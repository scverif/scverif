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
let hex     = digit | ['a'-'f'] | ['A'-'F']
let regident= ('r' digit+)
let ident   = char (char | digit)*
let immediate = ('#' hex+)

rule main = parse
  | newline        { Lexing.new_line lexbuf; main lexbuf}
  | blank+         { main lexbuf }
  | hex+ as h       { HEX h }
  | regident as r  { REGIDENT r }
  | immediate as i { IMMEDIATE i }
  | uint as n      { INT (int_of_string n) }
  | ident as id    { IDENT id }
  | "<"            { LT }
  | ">"            { GT }
  | "("            { LPAREN }
  | ")"            { RPAREN }
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
