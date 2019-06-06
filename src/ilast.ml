module L = Location
module B = Bigint

type wsize = 
  | U8 
  | U16 
  | U32 
  | U64

type bty = 
  | Bool
  | Int
  | W of wsize 
 
type ty = 
  | Tbase of bty
  | Tarr  of bty * B.zint * B.zint
  | Tmem   


type var = string L.located 

type label = string L.located 

type leak_info = string option

type macro = string L.located 

type op1 = 
  | OPP        of wsize option
  | NOT        of wsize option
  | SignExtend of wsize
  | ZeroExtend of wsize

type sign =
  | Signed 
  | Unsigned

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
  
type op = op_r L.located

type expr_desc = 
  | Eint  of B.zint 
  | Ebool of bool
  | Evar  of var 
  | Eget  of var * expr            (* array access *)
  | Eload of wsize * var * expr    (* memory access *)
  | Eop   of op * expr list

and expr = expr_desc L.located

type lval = 
  | Lvar   of var        
  | Lset   of var * expr           (* array assign *)
  | Lstore of wsize * var * expr   (* memory assign *)
  
type range = B.zint * B.zint 

type macro_arg = 
  | Avar   of var
  | Aindex of var * range

type instr_desc = 
  | Iassgn of lval * expr 
  | Ileak  of leak_info * expr list
  | Imacro of macro * macro_arg list 
  | Ilabel of label 
  | Igoto  of label
  | Iif    of expr * cmd * cmd 
  | Iwhile of cmd * expr * cmd 
  
and instr = instr_desc L.located

and cmd = instr list

type var_decl = {
  v_name : var;
  v_type : ty;
}

type param = 
  | Pvar   of var_decl 
  | Plabel of label 

type macro_decl = {
    mc_name   : string L.located;
    mc_params : param list;
    mc_locals  : param list;
    mc_body   : cmd;
  }

type command = 
  | Gvar   of var_decl L.located 
  | Gmacro of macro_decl L.located
  | Gexit  


