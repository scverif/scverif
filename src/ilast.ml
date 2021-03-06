(* Copyright 2019-2020 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

open Location
open Common

type t = [%import: Location.t]
[@@deriving show]

type 'a located = [%import: 'a Location.located]
[@@deriving show]

type ident = string located
[@@deriving show]

type label = {
  l_base : string;
  l_offs : B.zint;
  l_loc  : Location.t;
}

let pp_label fmt (l:label) =
  Format.fprintf fmt "%s+%a" l.l_base B.pp_print l.l_offs

let label_to_string (l:label) : string =
  l.l_base ^ (B.to_string l.l_offs)

type leak_info = string option
[@@deriving show]

type macro = string located
[@@deriving show]

type op1 =
  | OPP        of wsize option
  | NOT        of wsize option
  | SignExtend of wsize
  | ZeroExtend of wsize
  | Cast       of cast
[@@deriving show]

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
  | NAMECMP
  | LT  of sign
  | LE  of sign
[@@deriving show]

type op_r =
  | Op1 of op1
  | Op2 of op2 * wsize option
  | Opn of string
[@@deriving show]

type op = op_r located
[@@deriving show]

type expr_desc =
  | Eint  of B.zint
  | Ebool of bool
  | Evar  of ident
  | Eget  of ident * expr            (* array access *)
  | Eload of wsize * ident * expr    (* memory access *)
  | Eop   of op * expr list

and expr = expr_desc located
[@@deriving show]

type lval =
  | Lvar   of ident
  | Lset   of ident * expr           (* array assign *)
  | Lstore of wsize * ident * expr   (* memory assign *)
[@@deriving show]

type range = B.zint * B.zint
[@@deriving show]

type macro_arg =
  | Aexpr  of expr
  | Alabel of label
  | Aindex of ident * range
[@@deriving show]

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
[@@deriving show]

type var_decl = {
  v_name : ident;
  v_type : ty;
}
[@@deriving show]

type param =
  | Pvar   of var_decl located
  | Plabel of label
[@@deriving show]

type macro_decl = {
  mc_name   : string located;
  mc_params : param list;
  mc_locals : param list;
  mc_body   : cmd;
}
[@@deriving show]

type initval =
  | Iptr  of ident * B.zint
  | Ibool of bool
  | Iint  of B.zint
  | Iword of wsize * B.zint
  | Iarr  of initval list
  | Ilbl  of label
  | Iexit
[@@deriving show]

type io_ty =
  | Sharing
  | URandom
  | Public
  | Secret
[@@deriving show]

type region_elem = 
  | REvar of ident
  | REindex of ident * B.zint
  | RErange of ident * range
[@@deriving show]

type region_def = 
  | IOrange of range 
  | IOdef   of region_elem list
[@@deriving show]

type init_info =
  | Region  of ident * wsize * ident * range
  | Init    of ident * initval
  | Input   of io_ty * ident * region_def option
  | Output  of io_ty * ident * region_def option
[@@deriving show]

type eval_info = {
  eval_m : ident;
  eval_i : init_info list
}
[@@deriving show]

type read_kind =
  | Asm
  | Il
  | Gas
[@@deriving show]

type command =
  | Gvar         of var_decl located
  | Gmacro       of macro_decl located
  | Ginclude     of (read_kind * string located)
  | Gannotation  of eval_info
  | Gscvcmd      of Scv.scvval located list
  | Gexit
[@@deriving show]


(* ***************************************************** *)
let mk_var id =
  mk_loc (loc id) (Evar id)

let mk_int i =
  mk_loc (loc i) (Eint (unloc i))

let mk_cast_w ws e =
  let loc = loc e in
  mk_loc loc (Eop(mk_loc loc (Op1(Cast (CW ws))), [e]))
