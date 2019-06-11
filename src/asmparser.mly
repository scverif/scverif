%{
  open Location
  open Utils
  open Asmast
%}
%token <string> HEX
%token <string> REGIDENT
%token COLON COMMA PLUS
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token LT GT
%token <string> IDENT
%token <int> IMMEDIATE
%token EOF

%start section
%type <Asmast.section> section

%%

%inline address:
  | a=HEX
    { a }

%inline secname:
  | n=IDENT
    { n }

%inline instr_offset:
  | o=HEX
    { o }

%inline instr_binary:
  | ib=HEX
    { ib }

%inline instr_disasm:
  | id=IDENT
    { id }

%inline regident:
  | n=REGIDENT
    { Reg n }

%inline immediate:
  | i=IMMEDIATE
    { Imm i }

%inline regidentoffs:
  | LBRACKET n=REGIDENT COMMA i=immediate RBRACKET
    { RegOffs(n, i) }
  | LBRACKET n=REGIDENT COMMA m=REGIDENT RBRACKET
    { RegOffs(n, Reg m) }

regorimmoroffs:
  | r=regident
    { r }
  | i=immediate
    { i }
  | r=regidentoffs
    { r }

instr_rhs:
  | adr=address LT lid=IDENT PLUS loffs=HEX GT
    { [Label [adr; lid; loffs]] }
  | ops=separated_list(COMMA,regorimmoroffs)
    { ops }
  | LCURLY regs=separated_list(COMMA,regident) RCURLY
    { regs }
  | error { parse_error (Location.make $startpos $endpos) "" }

%inline stmt:
  | io=instr_offset COLON ib=instr_binary id=instr_disasm ir=instr_rhs
    { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }
  | error { parse_error (Location.make $startpos $endpos) "" }

section:
  | adr=address LT name=secname GT COLON stmts=list(stmt) EOF
    { { s_adr=adr; s_name=name; s_stmts=stmts } }
