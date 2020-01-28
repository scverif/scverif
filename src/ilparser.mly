(* Copyright 2019-2020 - Inria, NXP *)

%{
  open Location
  open Utils
  open Common
  open Ilast
  open Scv

%}

%token LPAREN RPAREN LCURLY RCURLY LBRACKET RBRACKET
%token LEFTARROW COLON SEMICOLON QUESTIONMARK COMMA EOF
%token MACRO LEAK IF ELSE WHILE LABEL GOTO
%token ANNOTATION INIT REGION VAR EXIT
%token SCVDOCSTART SCVDOCEND NULL
%token INPUT OUTPUT SECRET PUBLIC URANDOM SHARING
%token INCLUDE ASM IL GAS
%token BOOL TINT UINT W8 W16 W32 W64
%token ADD SUB MUL MULH AND XOR OR NOT EQ NEQ LSL LSR ASR ZEROEXTEND SIGNEXTEND TRUE FALSE NAMECMP
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
%start scvfile

%type <Ilast.command> command
%type <Ilast.command list> file
%type <Scv.scvval Location.located list> scvfile

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
  | v1=ident nc=loc(NAMECMP) v2=ident
    { Eop(mk_loc (loc nc) (Op2(Ilast.NAMECMP, None)),
          [mk_loc (loc v1) (Evar v1); mk_loc (loc v1) (Evar v2);]) }
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

label:
  | l=ident
    { {l_base = unloc l; l_offs = B.zero; l_loc = loc l} }
  | l=ident ADD i=loc(INT)
    { {l_base = unloc l; l_offs = unloc i; l_loc = Location.merge (loc l) (loc i)} }

instr_r:
  | x=lval LEFTARROW e=expr SEMICOLON
    { Iassgn(x,e) }
  | LEAK i=IDENT? LPAREN es=separated_list(COMMA,expr) RPAREN SEMICOLON
    { Ileak(i,es) }
  | m=ident args=macro_args SEMICOLON
    { Imacro(m,args) }
  | l=label COLON
    {Ilabel l }
  | GOTO l=label SEMICOLON
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
  | LABEL l=label { Plabel l }

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
  | ws=wsize i=INT                    { Iword(ws,i) }
  | LCURLY l=label RCURLY             { Ilbl l }
  | LBRACKET ivs=separated_list(COMMA, initval) RBRACKET
    { Iarr ivs }

annot_ty_kind:
  | SHARING                           { Sharing }
  | URANDOM                           { URandom }
  | PUBLIC                            { Public }
  | SECRET                            { Secret }

region_elem:
  | l=ident                         { REvar l }
  | l=ident LBRACKET i=INT RBRACKET { REget(l,i) }
  | l=ident r=range                 { REblock(l,r) }

region_def:
  | LBRACKET d=separated_list(SEMICOLON, region_elem) RBRACKET { d }

initialization:
  | VAR vd=loc(var_decl) { Var vd } 
  | REGION m=ident ws=wsize x=ident r=range rd=region_def? 
    { Region(m, ws, x, r, rd) }
  | INIT x=ident v=initval
    { Init(x,v) }
  | OUTPUT ty=annot_ty_kind i=ident r=range?
    { Output(ty, i, r) }
  | INPUT ty=annot_ty_kind i=ident r=range?
    { Input(ty, i, r) }

eval_command:
  | ANNOTATION m=ident i=initialization* SEMICOLON { {eval_m = m; eval_i = i } }

include_kind:
  | ASM { Asm }
  | IL  { Il }
  | GAS { Gas }

include_:
  | INCLUDE k=include_kind s=loc(STRING) { (k,s) }

%inline scvstringorident:
  | s=loc(STRING) { s }
  | i=loc(IDENT)  { i }

%inline scvvalueterminal:
  | s=scvstringorident { SCVString s } (* TODO locations *)
  | i=INT              { SCVInt i }
  | TRUE               { SCVBool true }
  | FALSE              { SCVBool false }
  | NULL               { SCVNull }

scvlist:
  | LBRACKET l=separated_list(COMMA, scvvalueterminal) RBRACKET
    { SCVList l }

scvmapentry:
  | k=scvstringorident COLON v=scvvalueterminal
    { SCVRecord [(k, v)] } (* associative list with single element *)
  | k=scvstringorident COLON v=scvlist
    { SCVRecord [(k, v)] } (* associative list with single element *)
  | m=scvmap
    { m }      (* nested variant (constructor call) *)

scvmap: (* variants consists of constructor name and named arguments (records) *)
  | k=scvstringorident COLON e=list(scvmapentry) SEMICOLON
    { SCVMap(k, e) }

%inline scvdoc_:
  | kv=list(loc(scvmap))
    { kv }

scvdoc:
  | SCVDOCSTART d=scvdoc_ SCVDOCEND { d }

command1:
  | x=loc(var_decl) SEMICOLON { Gvar   x }
  | m=loc(macro_decl)         { Gmacro m }
  | i=include_                { Ginclude i }
  | e=eval_command            { Gannotation e }
  | d=scvdoc                  { Gscvcmd d }
  | error        { parse_error (Location.make $startpos $endpos) "" }

command:
  | c= command1    { c }
  | EOF            { Gexit }

file:
  | c=command1* EOF { c }

scvfile:
  | d=scvdoc_ EOF { d }
