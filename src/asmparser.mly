%{
  open Location
  open Utils
  open Asmast
%}
%token COLON COMMA PLUS
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token LT GT
%token <string> IDENT
%token SHARP
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

%inline immediate_:
  | i=loc(IDENT) { mk_loc (loc i) (Bigint.of_string (unloc i)) }

%inline immediate:
  | SHARP i=immediate_ { i }

%inline instr_binary:
  | ib=loc(IDENT) { mk_loc (loc ib) (Bigint.of_string ("0x"^(unloc ib))) }

%inline instr_disasm:
  | id=loc(IDENT) { id }

%inline regident:
  | n=loc(IDENT) { n }


regimm:
  | r=regident  { Reg r }
  | i=immediate { Imm i }

operand:
  | ri=regimm                                     { Regimm ri }
  | LBRACKET r=regident COMMA ofs=regimm RBRACKET { RegOffs(r,ofs) } 
  | IDENT LT sid=loc(IDENT) PLUS loffs=immediate_ GT { Label(sid,loffs) } 

operands:
  | LCURLY regs=separated_list(COMMA,regident) RCURLY { Oflexible regs }
  | ops=separated_nonempty_list(COMMA, operand)       { Ofixed ops     }

stmt:
  | io=instr_binary COLON ib=instr_binary id=instr_disasm ir=operands
    { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }
  | error { parse_error (Location.make $startpos $endpos) "" }

section_r:
  | adr=instr_binary LT name=loc(secname) GT COLON stmts=list(loc(stmt)) EOF
    { { s_adr=adr; s_name=name; s_stmts=stmts } }

section:
  | s=loc(section_r)
    { s }
