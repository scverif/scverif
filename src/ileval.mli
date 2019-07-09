open Utils
open Common
open Il

type region = {
  r_from  : V.t;    (* name of the memory *)
  r_dest  : V.t;    (* variable destination *)
}

type ival =
  | Iint       of B.zint
  | Ibool      of bool
  | Iregion    of V.t * B.zint
  | Icptr_exit

type t_ty =
  | Sharing
  | URandom
  | Public
  | Secret

type initial = {
  init_region : region list;
  init_var    : (V.t * ival) list;
  input_var   : (t_ty * V.t) list;
  output_var  : (t_ty * V.t) list;
}

type pointer = {
  p_mem  : V.t;
  p_dest : V.t;
  p_ofs  : B.zint
}

type cpointer = Lbl.t

type bvalue =
  | Vcptr of Lbl.t
  | Vptr  of pointer
  | Vint  of B.zint
  | Vbool of bool
  | Vunknown

type value =
  | Varr  of bvalue array
  | Vbase of bvalue

type state = {
  mutable st_mregion : bvalue array Mv.t;
  mutable st_mvar    : value Mv.t;
          st_prog    : cmd;
  mutable st_pc      : cmd;
  mutable st_eprog   : cmd;
}

type eenv = {
  state   : state Ms.t;
  initial : initial Ms.t;
}

val empty_eenv : eenv

val pp_regions : Format.formatter -> bvalue array Il.Mv.t -> unit
val pp_vars    : Format.formatter -> value Il.Mv.t -> unit
val pp_state   : Format.formatter -> state -> unit
val pp_initial : Format.formatter -> initial -> unit

val find_state     : eenv -> string -> state
val update_state   : eenv -> macro -> state -> eenv
val find_initial   : eenv -> string -> initial
val update_initial : eenv -> macro -> initial -> eenv

val partial_eval : eenv -> macro -> state
