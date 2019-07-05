open Location
open Common

type t = [%import: Location.t]
[@@deriving show]

type 'a located = [%import: 'a Location.located]
[@@deriving show]

type ident = string located
[@@deriving show]

type label = string located
[@@deriving show]

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
  | Pvar   of var_decl  located
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
  | Iexit
[@@deriving show]

type init_info =
  | Region of ident * wsize * ident * range
  | Init   of ident * initval
[@@deriving show]

type eval_info = {
  eval_m : ident;
  eval_i : init_info list
}
[@@deriving show]

type apply_info = {
  apply_t  : ident;
  apply_ms : ident list
}
[@@deriving show]

type read_kind =
  | Asm
  | Il
[@@deriving show]

type command =
  | Gvar   of var_decl located
  | Gmacro of macro_decl located
  | Ginclude of (read_kind * string located)
  | Gannotation  of eval_info
  | Gapply of apply_info
  | Gverbose of int
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
