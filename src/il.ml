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
  | Bty of bty
  | Barr of bty * int * int
  | Bmem   


type var = {
    v_name : string;
    v_id   : Uid.t;
    v_ty   : ty;
    v_rnd  : bool;  (* True if the variable is a random *)
  }

type label = {
    l_name : string;
    l_id   : Uid.t;
  }

type leak_info = string option

type macro = {
    m_name : string;
    m_id   : Uid.t;
  }

type op_desc = 
  | Oif
  | Oadd
  | Omul
  | Osub
  | Oopp
  | Oand
  | Oxor
  | Oor
  | Onot
  | Oeq
  | Oneq
  | Olt
  | Ole
  | Ogt
  | Oge

type op = {
    od   : op_desc;
    oinv : bool;  (* true if invertible *) 
  }

type expr = 
  | Eint of B.zint 
  | Ebool of bool
  | Evar of var 
  | Eget of var * expr            (* array access *)
  | Eload of wsize * var * expr   (* memory access *)
  | Eop  of op * expr list

type lval = 
  | Lvar   of var        
  | Lset   of var * expr           (* array assign *)
  | Lstore of wsize * var * expr   (* memory assign *)
  
type macro_arg = 
  | Avar   of var
  | Alabel of label
  | Aindex of var * int * int   

type instr_info = L.t * L.t list

type instr_desc = 
  | Iassgn of lval * expr 
  | Ileak  of leak_info * expr list
  | Imacro of macro * macro_arg list 
  | Ilabel of label 
  | Igoto  of label
  | Iif    of expr * cmd * cmd 
  | Iwhile of cmd * expr * cmd 
  
and instr = {
   id : instr_desc;
   ii : instr_info;
  }

and cmd = instr list

type param = 
  | Pvar of var 
  | Plabel of label 

type macro_decl = {
    mc_name : string;
    mc_loc  : L.t;
    mc_params : param list;
    mc_local  : param list;
    mc_body   : cmd;
  }

type global = 
  | Gvar   of var 
  | Gmacro of macro_decl


