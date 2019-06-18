%{
  open Location
  open Utils
  open Asmast
%}
%token <Bigint.zint> HEX
%token <string> REGIDENT
%token COLON COMMA PLUS
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token LT GT
%token <string> IDENT
%token <Bigint.zint> IMMEDIATE
%token EOF

%start section
%type <Asmast.section> section

%%

%inline loc(X):
  | x=X
    { { pl_desc = x; pl_loc = Location.make $startpos $endpos; } }

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
  | n=loc(REGIDENT)
    { Reg n }

%inline immediate:
  | i=loc(IMMEDIATE)
    { Imm i }

%inline regidentoffs:
  | LBRACKET n=loc(REGIDENT) COMMA i=immediate RBRACKET
    { RegOffs(n, i) }
  | LBRACKET n=loc(REGIDENT) COMMA m=loc(REGIDENT) RBRACKET
    { RegOffs(n, Reg m) }

regorimmoroffs:
  | r=regident
    { r }
  | i=immediate
    { i }
  | r=regidentoffs
    { r }

instr_rhs:
  | adr=loc(HEX) LT lid=loc(IDENT) PLUS loffs=loc(HEX) GT
    { [Label [Hex adr; NLabel lid; Hex loffs]] }
  | ops=separated_list(COMMA,regorimmoroffs)
    { ops }
  | LCURLY regs=separated_list(COMMA,regident) RCURLY
    { regs }
  | error { parse_error (Location.make $startpos $endpos) "" }

stmt:
  | io=loc(instr_offset) COLON ib=loc(instr_binary) id=instr_disasm ir=instr_rhs
    { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }
  | error { parse_error (Location.make $startpos $endpos) "" }

section_r:
  | adr=loc(HEX) LT name=loc(secname) GT COLON stmts=list(loc(stmt)) EOF
    { { s_adr=adr; s_name=name; s_stmts=stmts } }

section:
  | s=loc(section_r)
    { s }
