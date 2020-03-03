(* Copyright 2019 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear *)

%{
  open Location
  open Utils
  open Asmast
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

%inline secname:
  | n=IDENT
    { n }

%inline hexseq:
  | hs=loc(HEX)
    { mk_loc (loc hs) (String.concat "" (String.split_on_char ' ' (unloc hs)))}

%inline immediate_:
  | i=loc(IDENT)
    { mk_loc (loc i) (Bigint.of_string (unloc i)) }
  | h=loc(HEX) (* four digit immediate ambiguously classified as hex *)
    { mk_loc (loc h) (Bigint.of_string (unloc h)) }

%inline immediate:
  | SHARP i=immediate_ { i }

hex:
  | ib=hexseq
    { mk_loc (loc ib)
             (try Bigint.of_string ("0x"^(unloc ib))
              with Invalid_argument _ -> Bigint.of_string (unloc ib)) }

hex_vl:
  | ib=loc(IDENT)
    { mk_loc (loc ib) (Bigint.of_string ("0x"^(unloc ib))) }
  | h=hex
    { h }

%inline instr_disasm:
  | id=loc(IDENT) { id }

%inline instr_disasm_excl:
  | ib=loc(IDENT) { mk_loc (loc ib) ((unloc ib)^"excl") }

%inline regident:
  | n=loc(IDENT) { n }

regimm:
  | r=regident  { Reg r }
  | i=immediate { Imm i }
  | i=hex       { Imm i }

operand:
  | ri=regimm
    { Regimm ri }
  | LBRACKET r=regident COMMA ofs=regimm RBRACKET
    { RegOffs(r,ofs) }
  | hex_vl LT sid=loc(IDENT) PLUS loffs=immediate_ GT
    { Label(sid,loffs) }
  | hex_vl LT sid=loc(IDENT) GT
    { Label(sid, mk_loc (loc sid) (Bigint.zero)) }

%inline commoreol:
  | COMMENT {}
  | EOL     {}

operands:
  | LCURLY regs=separated_list(COMMA,regident) RCURLY commoreol
    { Oflexible regs }
  | ops=separated_nonempty_list(COMMA, operand) commoreol
    { Ofixed ops }
  | COMMENT
    { Onone }

%inline operands_excl:
  | ra=regident EXCLAMATION COMMA LCURLY regs=separated_list(COMMA,regident) RCURLY commoreol
    { Oflexible (List.cons ra regs) }

stmt:
  | io=hex_vl COLON ib=hex id=instr_disasm ir=operands
    { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }
  | io=hex_vl COLON ib=hex id=instr_disasm_excl ir=operands_excl
    { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }
  | error { parse_error (Location.make $startpos $endpos) "asmparser: stmt invalid" }

section_r:
  | adr=hex_vl LT name=loc(secname) GT COLON EOL stmts=list(loc(stmt))
    { { s_adr=adr; s_name=name; s_stmts=stmts } }
  | error { parse_error (Location.make $startpos $endpos) "asmparser: invalid section header" }

section:
  | commoreol* s=loc(section_r) EOF
    { s }

sections:
  | commoreol* ls=separated_nonempty_list(commoreol+, loc(section_r)) EOF
    { ls }
