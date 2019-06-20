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
    { n }

%inline immediate:
  | i=loc(IMMEDIATE)
    { i }

regimm:
  | r=regident  { Reg r }
  | i=immediate { Imm i }

operand:
  | ri=regimm                                     { Regimm ri }
  | LBRACKET r=regident COMMA ofs=regimm RBRACKET { RegOffs(r,ofs) }
  | HEX  LT sid=loc(IDENT) PLUS loffs=loc(HEX) GT { Label(sid,loffs) }

operands:
  | LCURLY regs=separated_list(COMMA,regident) RCURLY { Oflexible regs }
  | ops=separated_nonempty_list(COMMA, operand)       { Ofixed ops     }

stmt:
  | io=loc(instr_offset) COLON ib=loc(instr_binary) id=instr_disasm ir=operands
    { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }
  | error { parse_error (Location.make $startpos $endpos) "" }

section_r:
  | adr=loc(HEX) LT name=loc(secname) GT COLON stmts=list(loc(stmt)) EOF
    { { s_adr=adr; s_name=name; s_stmts=stmts } }

section:
  | s=loc(section_r)
    { s }
