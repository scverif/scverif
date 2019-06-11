open Location
open Common

type ident = string located 

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
  | Evar  of ident 
  | Eget  of ident * expr            (* array access *)
  | Eload of wsize * ident * expr    (* memory access *)
  | Eop   of op * expr list

and expr = expr_desc located

type lval = 
  | Lvar   of ident        
  | Lset   of ident * expr           (* array assign *)
  | Lstore of wsize * ident * expr   (* memory assign *)
  
type range = B.zint * B.zint 

type macro_arg = 
  | Aexpr  of expr
  | Aindex of ident * range

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
  v_name : ident;
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

type initval = 
  | Iptr  of ident * B.zint
  | Ibool of bool
  | Iint  of B.zint 
  | Iexit 

type init_info = 
  | Region of ident * wsize * ident * range 
  | Init   of ident * initval 

type eval_info = { eval_m : ident; eval_i : init_info list }

type command = 
  | Gvar   of var_decl located 
  | Gmacro of macro_decl located
  | Geval  of eval_info 
  | Gexit  


