%{
  open Asmast

%}
%token <string> HEX
%token <string> REGIDENT
%token COLON COMMA HASHTAG
%token LBRACKET RBRACKET
%token LCURLY RCURLY
%token LT GT
%token <string> IDENT
%token EOL EOF

%start section
%type <Asmast.section> section

%%

address:
  | a=HEX
    { a }

secname:
  | n=IDENT
    { n }

instr_offset:
  | o=HEX
    { o }

instr_binary:
  | ib=HEX
    { ib }

instr_disasm:
  | id=IDENT
    { id }

regident:
  | n=REGIDENT
    { Reg n }
  | LBRACKET n=REGIDENT COMMA HASHTAG s=HEX RBRACKET
    { RegShift (n, s); }

instr_rhs:
  | regs=separated_list(COMMA,regident)
    { regs }
  | LCURLY regs=separated_list(COMMA,regident) RCURLY
    { regs }

stmt:
  | io=instr_offset COLON ib=instr_binary id=instr_disasm ir=instr_rhs EOL { { offset=io; instr_bin=ib; instr_asm=id; instr_exp=ir } }

section:
  | adr=address LT name=secname GT COLON EOL
      stmts=list(stmt)
    EOF
    { { s_adr=adr; s_name=name; s_stmts=stmts } }
