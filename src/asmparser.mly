(* Copyright 2019 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

%{
  open Location
  open Utils
  open Asmast
  open Common
%}
%token COLON COMMA PLUS
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token LT GT
%token <string> IDENT HEX COMMENT
%token SHARP EXCLAMATION
%token EOL EOF

%start section sections
%type <Asmast.section> section
%type <Asmast.section list> sections

%%

%inline loc(X):
  | x=X
    { { pl_desc = x; pl_loc = Location.make $startpos $endpos; } }

%inline ident:
  | n=loc(IDENT)
    { n }

%inline hexseq:
  | hs=loc(HEX)
    { mk_loc (loc hs) (String.concat "" (String.split_on_char ' ' (unloc hs)))}

immediate:
  | i=loc(IDENT)
    { mk_loc (loc i) (B.of_string (unloc i)) }
  | h=loc(HEX) (* four digit immediate ambiguously classified as hex *)
    { mk_loc (loc h) (B.of_string (unloc h)) }

hex:
  | ib=hexseq
    { mk_loc (loc ib)
             (try B.of_string ("0x"^(unloc ib))
              with Invalid_argument _ -> B.of_string (unloc ib)) }

hex_vl:
  | ib=loc(IDENT)
    { mk_loc (loc ib) (B.of_string ("0x"^(unloc ib))) }
  | h=hex
    { h }

commoreol:
  | COMMENT {}
  | EOL     {}

%inline offset:
  | PLUS ofs=immediate { Some ofs }
  |                    { None }

operand_simple:
  | r=ident
    { Reg r }
  | SHARP i=immediate
    { Imm i }
  | hex_vl LT sid=loc(IDENT) ofs=offset GT
    { Label(sid, ofs) }

operand:
  | o=operand_simple
    { [o] }
  | LBRACKET r=separated_list(COMMA, operand_simple) RBRACKET
    { r }

operandregion:
  | { [] }
  | o=operand_simple
    { [o] }
  | r=ident COMMA ops=separated_nonempty_list(COMMA, operand)
    { (Reg r)::(List.flatten ops) }
  | LCURLY rs=separated_list(COMMA, operand_simple) RCURLY
    { rs }
  (* special cases of "rX!, ..." with optional exclamation mark "!" *)
  | r=ident COMMA LCURLY rs=separated_list(COMMA, operand_simple) RCURLY
    { (Reg r)::(Bool (mk_loc (loc r) false))::rs }
  | r=ident l=loc(EXCLAMATION) COMMA LCURLY rs=separated_list(COMMA, operand_simple) RCURLY
    { (Reg r)::(Bool (mk_loc (loc l) true))::rs }
  | error { parse_error (Location.make $startpos $endpos) "asmparser: invalid instruction operands" }

stmt:
  | io=hex_vl COLON ib=hex id=ident ir=operandregion commoreol
    { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }
  | error { parse_error (Location.make $startpos $endpos) "asmparser: assembly line invalid" }

section_r:
  | adr=hex_vl LT name=ident GT COLON EOL stmts=list(loc(stmt))
    { { s_adr=adr; s_name=name; s_stmts=stmts } }
  | error { parse_error (Location.make $startpos $endpos) "asmparser: invalid assembly section header" }

section:
  | commoreol* s=loc(section_r) EOF
    { s }

sections:
  | commoreol* ls=separated_nonempty_list(commoreol+, loc(section_r)) EOF
    { ls }
