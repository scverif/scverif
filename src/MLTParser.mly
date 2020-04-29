(* Copyright 2014-2020 - Inria *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)
(* -------------------------------------------------------------------- *)
%{
  open MLTAst

  let parse_error loc msg = raise (ParseError (loc, msg))
%}

%token EOF

%token LPAREN RPAREN LBRACE RBRACE LANGLE RANGLE LBRACKET RBRACKET 
%token LLANGLE LEQ RRANGLE GEQ
%token EQUAL EQEQ COMMA SEMICOLON
%token PLUS MINUS MUL DIV MOD HAT AMP BAR PLUSPLUS MINUSMINUS TILDE
%token PLUSEQ MINUSEQ HATEQ AMPEQ BAREQ LLANGEQ RRANGEQ

%token BINT8_T BINT16_T BINT32_T BINT64_T 
%token UINT8_T UINT16_T UINT32_T UINT64_T 
%token INT8_T  INT16_T  INT32_T  INT64_T  INT_T
%token VOID

%token IF ELSE FOR RETURN

%token <MLTAst.symbol  > IDENT
%token <Big_int_Z.big_int> INT

%right EQUAL PLUSEQ MINUSEQ HATEQ AMPEQ BAREQ (* C: priority 14 *)
       LLANGEQ RRANGEQ                

%left  BAR                            (* C: priority 10 *)
%left  HAT                            (* C: priority  9 *)
%left  AMP                            (* C: priority  8 *)
%left  EQEQ                           (* C: priority  7 *)
%left  LANGLE LEQ RANGLE GEQ          (* C: priority  6 *)
%left  LLANGLE RRANGLE                (* C: priority  5 *)
%left  PLUS MINUS                     (* C: priority  4 *)
%left  MUL DIV MOD                    (* C: priority  3 *)
%nonassoc TILDE







%nonassoc IF
%nonassoc ELSE

%type <MLTAst.pprogram> main

%start main

%%
(* -------------------------------------------------------------------- *)
(* Wrappers around tokens                                               *)
%inline int  : i=loc(INT)   { i }
%inline ident: x=loc(IDENT) { x }

(* -------------------------------------------------------------------- *)
(* Type expressions                                                     *)
coretype_:
| VOID     { TVoid }
| BINT8_T  { TSec (`Boolean uint8_t)  }
| BINT16_T { TSec (`Boolean uint16_t) }
| BINT32_T { TSec (`Boolean uint32_t) }
| BINT64_T { TSec (`Boolean uint64_t) }
| UINT8_T  { TInt uint8_t             }
| UINT16_T { TInt uint16_t            }
| UINT32_T { TInt uint32_t            }
| UINT64_T { TInt uint64_t            }
| INT8_T   { TInt int8_t              }
| INT16_T  { TInt int16_t             }
| INT32_T  { TInt int32_t             }
| INT64_T  { TInt int64_t             }
| INT_T    { TInt int_t               }
;

type_:
| x=loc(coretype_) { x }
;

(* -------------------------------------------------------------------- *)
(* Expressions                                                          *)
%inline binop:
| PLUS    { `Plus   }
| MINUS   { `Minus  }
| MUL     { `Mul   }
| DIV     { `Div    }
| AMP     { `Amp    }
| HAT     { `Hat    }
| BAR     { `Bar    }
| EQEQ    { `Eq     }
| LANGLE  { `Lt     }
| LEQ     { `Le     }
| RANGLE  { `Gt     }
| GEQ     { `Ge     }
| MOD     { `Mod    }
| LLANGLE { `LShift }
| RRANGLE { `RShift }
;

%inline uniop:
| TILDE   { `Tilde }

%inline asgop:
| EQUAL   { None           }
| PLUSEQ  { Some `Plus     }
| MINUSEQ { Some `Minus    }
| AMPEQ   { Some `Amp      }
| HATEQ   { Some `Hat      }
| BAREQ   { Some `Bar      }
| LLANGEQ { Some `LShiftEq }
| RRANGEQ { Some `RShiftEq }
;

lexpression_r:
| x=ident                                      { LEIdent x }
| t=lexpression LBRACKET e=expression RBRACKET { LEArr(t, e) }
;

sexpression_r:
| x=lexpression
    { PEIdent x }

| i=int
    { PEInt (data i) }

| LPAREN e=expression RPAREN
    { PEParens e }
;

expression_r:
| e=sexpression_r
    { e }

| x=lexpression op=asgop e=expression
    { PEAssign (x, op, e) }

| f=ident args=parens(plist0(expression, COMMA))
    { PECall (f, args) }

| e1=expression binop=binop e2=expression
    { PEBinOp (binop, (e1, e2)) }

| uniop=uniop e1=expression
    { PEUniOp (uniop, e1) }

| e=loc(lexpression) PLUSPLUS
    { PEUniOp (`PostIncr, mkloc (loc e) (PEIdent (data e))) }

| e=loc(lexpression) MINUSMINUS
    { PEUniOp (`PostDecr, mkloc (loc e) (PEIdent (data e))) }

| LBRACE es=plist2(expression, COMMA) RBRACE
   { PEArrDef es }
;

lexpression:
| e = loc(lexpression_r) { e }
;

expression:
| e=loc(expression_r) { e }
;

(* -------------------------------------------------------------------- *)
(* Function definition                                                  *)
function_r:
| retty=type_ name=ident
    args=parens(plist0(function_arg, COMMA))
    body=braces(function_body)
  { { fc_name  = name;
      fc_args  = args;
      fc_retty = retty;
      fc_decls = (fst body);
      fc_body  = (snd body); } }
;

function_:
| x=loc(function_r) { x }
;

function_arg:
| ty=type_ x=sized_var
    { (x, ty) }
;

function_body:
| decls=loc(localdecl)* is=instruction*
    { (decls, is) }
;

localdecl:
| ty=type_ vars=plist1(var_with_oinit, COMMA) SEMICOLON
    { { ld_type = ty; ld_vars = vars; } }
;

size:
| LBRACKET i=int RBRACKET { i }
;
sized_var:
| x=ident s=size*  { (x,s)}
;


var_with_oinit:
| x=sized_var                    { (x, None  ) }
| x=sized_var EQUAL e=expression { (x, Some e) }
;

(* -------------------------------------------------------------------- *)
instruction_r:
| e=expression SEMICOLON
    { PIExpression e }

| FOR LPAREN
    e1=expression? SEMICOLON
    e2=expression? SEMICOLON
    e3=expression?
  RPAREN body=block_or_instruction
    { PIFor { pfi_init = e1  ;
              pfi_test = e2  ;
              pfi_post = e3  ;
              pfi_body = body; } }

| IF e=parens(expression) i=block_or_instruction %prec IF
    { PIIf { pii_test = e; pii_then = i; pii_else = None; } }

| IF e=parens(expression)
         i1=block_or_instruction
    ELSE i2=block_or_instruction
    { PIIf { pii_test = e; pii_then = i1; pii_else = Some i2; } }

| RETURN e=expression? SEMICOLON
    { PIReturn e }
;

instruction:
| i=loc(instruction_r) { i }
;

block:
| stmt=braces(instruction*) { stmt }
;

block_or_instruction:
| stmt=block        { stmt    }
| instr=instruction { [instr] }
;

(* -------------------------------------------------------------------- *)
(* Program                                                              *)
global_r:
| f=function_ { PFun f }
| ty=type_ x=sized_var EQUAL e=expression SEMICOLON { PStatic(ty,x,e) }
;
global:
| g=loc(global_r) { g }
;

program: funs=global* { funs };

(* -------------------------------------------------------------------- *)
(* Entry point                                                          *)
main:
| program=program EOF
    { program }

| error
    { parse_error (Location.make $startpos $endpos) None }
;

(* -------------------------------------------------------------------- *)
(* Combinators                                                          *)
%inline parens(X): LPAREN x=X RPAREN { x };
%inline braces(X): LBRACE x=X RBRACE { x };

%inline plist0(X, S):
| aout=separated_list(S, X) { aout }
;

%inline plist1(X, S):
| aout=separated_nonempty_list(S, X) { aout }
;

%inline plist2(X, S):
| x=X S aout=separated_nonempty_list(S, X) { x::aout }
;

(* -------------------------------------------------------------------- *)
(* Localized rules                                                      *)
%inline loc(X):
| x=X { { pl_data = x; pl_location = Location.make $startpos $endpos; } }
;
