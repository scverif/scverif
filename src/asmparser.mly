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
    { n }
  | LBRACKET n=REGIDENT COMMA HASHTAG s=HEX RBRACKET
    { {n; s} }

instr_rhs:
  | regs=separated_list(COMMA,regident)
    { regs }
  | LCURLY regs=separated_list(COMMA,regident) RCURLY
    { regs }

stmt:
  | io=instr_offset COLON ib=instr_binary id=instr_disasm ir=instr_rhs EOL { { io; ib; id; ir} }

section:
  | s_adr=address LT s_name=secname GT COLON EOL
      s_stmt=stmt
    EOF
    { let sd = { s_adr; s_name; s_stmt } in Section sd}
