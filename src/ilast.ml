open Location
open Common

type var = string located 

type label = string located 

type leak_info = string option

type macro = string located 

type op1 = 
  | OPP        of wsize option
  | NOT        of wsize option
  | SignExtend of wsize
  | ZeroExtend of wsize
  | Cast       of cast

type op2 = 
  | ADD 
  | SUB 
  | MUL 
  | MULH 
  | LSL 
  | LSR 
  | ASR
  | AND
  | XOR
  | OR
  | EQ
  | NEQ
  | LT  of sign
  | LE  of sign

type op_r = 
  | Op1 of op1
  | Op2 of op2 * wsize option
  | Opn of string 
  
type op = op_r located

type expr_desc = 
  | Eint  of B.zint 
  | Ebool of bool
  | Evar  of var 
  | Eget  of var * expr            (* array access *)
  | Eload of wsize * var * expr    (* memory access *)
  | Eop   of op * expr list

and expr = expr_desc located

type lval = 
  | Lvar   of var        
  | Lset   of var * expr           (* array assign *)
  | Lstore of wsize * var * expr   (* memory assign *)
  
type range = B.zint * B.zint 

type macro_arg = 
  | Aexpr  of expr
  | Aindex of var * range

type instr_desc = 
  | Iassgn of lval * expr 
  | Ileak  of leak_info * expr list
  | Imacro of macro * macro_arg list 
  | Ilabel of label 
  | Igoto  of label
  | Iif    of expr * cmd * cmd 
  | Iwhile of cmd * expr * cmd 
  
and instr = instr_desc located

and cmd = instr list

type var_decl = {
  v_name : var;
  v_type : ty;
}

type param = 
  | Pvar   of var_decl  located
  | Plabel of label 

type macro_decl = {
    mc_name   : string located;
    mc_params : param list;
    mc_locals : param list;
    mc_body   : cmd;
  }

type command = 
  | Gvar   of var_decl located 
  | Gmacro of macro_decl located
  | Gexit  


