%{
  open Location
  open Utils
  open Common
  open Ilast

%}

%token LPAREN RPAREN LCURLY RCURLY LBRACKET RBRACKET
%token LEFTARROW COLON SEMICOLON QUESTIONMARK COMMA EOF
%token MACRO LEAK IF ELSE WHILE LABEL GOTO TRACE STATE
%token ANNOTATION INIT REGION EXIT APPLY PRINT OUTCOME
%token INCLUDE ASM IL VERBOSE
%token BOOL TINT UINT W8 W16 W32 W64
%token ADD SUB MUL MULH AND XOR OR NOT EQ NEQ LSL LSR ASR ZEROEXTEND SIGNEXTEND TRUE FALSE
%token <Common.sign> LT
%token <Common.sign> LE
%token <Bigint.zint> INT
%token <string>IDENT
%token <string>STRING

%nonassoc COLON QUESTIONMARK
%left EQ NEQ LT LE
%left ASR LSL LSR
%left ADD SUB XOR OR
%left MUL MULH AND
%nonassoc NOT

%start command
%start file

%type <Ilast.command> command
%type <Ilast.command list> file

%%

wsize:
  | W8    { U8  }
  | W16   { U16 }
  | W32   { U32 }
  | W64   { U64 }

base_type:
  | BOOL    { Bool  }
  | TINT    { Int   }
  | w=wsize { W w   }

int:
  | i=INT     { i }
  | SUB i=INT { B.neg i }

%inline loc(X):
  | x=X { { pl_desc = x; pl_loc = Location.make $startpos $endpos; } }

ident:
  | x=loc(IDENT) { x }

%inline op2_r:
  | ADD    { Ilast.ADD }
  | SUB    { Ilast.SUB }
  | MUL    { Ilast.MUL }
  | MULH   { Ilast.MULH }
  | LSR    { Ilast.LSR }
  | LSL    { Ilast.LSL }
  | ASR    { Ilast.ASR }
  | AND    { Ilast.AND }
  | XOR    { Ilast.XOR }
  | OR     { Ilast.OR }

  | EQ     { Ilast.EQ }
  | NEQ    { Ilast.NEQ }
  | s=LT   { Ilast.LT s }
  | s=LE   { Ilast.LE s }

%inline op2:
  | o=op2_r s=wsize? { o,s }

%inline op1:
  | SUB s=wsize?  { OPP s }
  | NOT s=wsize?  { NOT s }
  | SIGNEXTEND s=wsize { SignExtend s }
  | ZEROEXTEND s=wsize { ZeroExtend s }

%inline question_r:
   | QUESTIONMARK { Opn "if" }

%inline question:
   | l=loc(question_r) { l }

cast:
   | s=wsize { CW s }
   | TINT    { Cint Signed }
   | UINT    { Cint Unsigned }

expr_r:
  | x=ident                                   { Evar x }
  | TRUE                                      { Ebool true }
  | FALSE                                     { Ebool false }
  | i=INT                                     { Eint i }
  | x=ident LBRACKET e=expr RBRACKET          { Eget(x,e) }
  | LBRACKET s=wsize x=ident e=expr RBRACKET  { Eload(s, x, e) }
  | e1=expr o=loc(op2) e2=expr
    { Eop(Location.lmap (fun (o,s) -> Op2(o,s)) o, [e1;e2]) }
  | o=loc(op1) e=expr    %prec NOT
    { Eop(Location.lmap (fun o -> Op1 o) o, [e]) }
  | o=ident LPAREN es=separated_nonempty_list(COMMA,expr) RPAREN
    { Eop(Location.lmap (fun o -> Opn o) o,es) }
  | e=expr o=question e1=expr COLON e2=expr   { Eop(o, [e;e1;e2]) }
  | LPAREN e=expr RPAREN { unloc e }
  | LPAREN c=loc(cast) RPAREN e=expr %prec NOT
    { Eop(Location.lmap (fun c -> Op1 (Cast c)) c, [e]) }

expr:
  | e=loc(expr_r) { e }

%inline range:
  | LBRACKET i1=int COLON i2=int RBRACKET { (i1,i2) }

macro_arg:
  | e=expr { Aexpr e }
  | LBRACKET x=ident i1=int COLON i2=int RBRACKET { Aindex(x,(i1,i2)) }

macro_args:
  | LPAREN es=separated_list(COMMA, macro_arg) RPAREN { es }

lval:
  | x=ident                                  { Lvar x }
  | x=ident LBRACKET e=expr RBRACKET         { Lset(x,e) }
  | LBRACKET s=wsize x=ident e=expr RBRACKET { Lstore(s, x, e) }

instr_r:
  | x=lval LEFTARROW e=expr SEMICOLON
    { Iassgn(x,e) }
  | LEAK i=IDENT? LPAREN es=separated_list(COMMA,expr) RPAREN SEMICOLON
    { Ileak(i,es) }
  | m=ident args=macro_args SEMICOLON
    { Imacro(m,args) }
  | l=ident COLON
    {Ilabel l }
  | GOTO l=ident SEMICOLON
    { Igoto l }
  | IF e=expr c1=cmd c2=else_?
    { Iif(e,c1,Utils.odfl [] c2) }
  | WHILE c1=cmd? LPAREN e=expr RPAREN c2=cmd?
    { Iwhile(Utils.odfl [] c1, e, Utils.odfl [] c2) }

instr:
  | i=loc(instr_r) { i }

%inline else_:
  | ELSE c=cmd { c }

cmd:
  | LCURLY c=instr* RCURLY { c }

var_decl:
  | x=ident LBRACKET RBRACKET  { {v_name = x; v_type = Tmem } }
  | bty=base_type x=ident      { {v_name = x; v_type = Tbase bty } }
  | bty=base_type x=ident LBRACKET i1=int COLON i2=int RBRACKET
                              { {v_name = x; v_type = Tarr(bty,i1,i2) } }

param_decl:
  | x=loc(var_decl)    { Pvar x }
  | LABEL x=ident { Plabel x }

macro_decl:
  | MACRO x=ident LPAREN p=separated_list(COMMA,param_decl) RPAREN
    l=separated_list(COMMA,param_decl)
    c = cmd
    { { mc_name = x;  mc_params = p; mc_locals = l; mc_body = c } }


initval:
  | LBRACKET r=ident ofs=int RBRACKET { Iptr(r, ofs) }
  | TRUE                              { Ibool true }
  | FALSE                             { Ibool false }
  | i=INT                             { Iint i }
  | EXIT                              { Iexit }

initialization:
  | REGION m=ident ws=wsize x=ident r=range
    { Region(m, ws, x, r) }
  | INIT x=ident v=initval
    { Init(x,v) }
  | OUTCOME x=ident
    { Outcome x }

eval_command:
  | ANNOTATION m=ident i=initialization* SEMICOLON { {eval_m = m; eval_i = i } }

applicationlist:
  | is=separated_list(COMMA,ident)
    { is }

apply_command:
  | APPLY t=ident ms=applicationlist SEMICOLON
    { {apply_t = t; apply_ms = ms } }

printlist:
  | MACRO id=ident
    { { p_pk = Macro; p_id = id} }
  | INIT id=ident
    { { p_pk = InitialEnv; p_id = id} }
  | STATE id=ident
    { { p_pk = State; p_id = id} }
  | TRACE id=ident
    { { p_pk = EvalTrace; p_id = id} }

print_command:
  | PRINT ps=printlist* SEMICOLON
    { ps }

include_kind:
  | ASM { Asm }
  | IL  { Il }

include_:
  | INCLUDE k=include_kind s=loc(STRING) { (k,s) }

command1:
  | x=loc(var_decl) SEMICOLON { Gvar   x }
  | m=loc(macro_decl)         { Gmacro m }
  | i=include_                { Ginclude i }
  | e=eval_command            { Gannotation e }
  | a=apply_command           { Gapply a }
  | p=print_command           { Gprint p }
  | VERBOSE i=INT             { Gverbose (B.to_int i) }
  | error        { parse_error (Location.make $startpos $endpos) "" }

command:
  | c= command1    { c }
  | EOF            { Gexit }

file:
  | c=command1* EOF { c }
