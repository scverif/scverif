(* Copyright 2019-2020 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

open Utils
open Common
open Il

type region = {
  r_from  : V.t;    (** name of the memory *)
  r_dest  : V.t;    (** variable destination *)
}

type ival =
  | Iint       of B.zint
  | Ibool      of bool
  | Iarr       of ival list
  | Ilbl       of Lbl.t
  | Iptr       of V.t * B.zint
  | Icptr_exit

type t_ty =
  | Sharing
  | URandom
  | Public
  | Secret

(** resource declaration *)
type rdecl =
  | RDvar of var          (** scalar variable *)
  | RDget of var * B.zint (** variable accessed at specific position *)

type rdecls = rdecl array

(** initial state and IO taint specification *)
type initial = {
  init_region : region list;                (** memory layout,
                                                mapping region-variables to memory-variables *)
  init_var    : (V.t * ival) list;          (** initial values of variables,
                                                can correspond to region-variables *)
  input_var   : (t_ty * V.t * rdecls) list; (** taint of input  variables and their resource *)
  output_var  : (t_ty * V.t * rdecls) list; (** taint of output variables and their resource *)
}

type pointer = {
  p_mem  : V.t;   (** access to this memory is performed *)
  p_dest : V.t;   (** the variable representing the accessed location *)
  p_ofs  : B.zint (** index of the access relative to p_dest *)
}

type cpointer = Lbl.t

type bvalue =
  | Vcptr of cpointer
  | Vptr  of pointer
  | Vint  of B.zint
  | Vbool of bool
  | Vunknown

type value =
  | Varr  of bvalue array
  | Vbase of bvalue

(** state for partial evaluation *)
type state = {
  mutable st_mregion : bvalue array Mv.t;  (** store containing value of memories *)
  mutable st_mvar    : value Mv.t;         (** store containing value of variables *)
  mutable st_prog    : cmd;                (** instructions of current scope (e.g. macro)*)
  mutable st_pc      : cmd;                (** instructions to be executed next *)
  mutable st_eprog   : cmd;                (** trace of evaluated instructions *)
          st_global  : Il.genv;            (** copy of global environment *)
          st_lblvar  : V.t option;         (** variable always holding the last visited label *)
}

type eenv = {
  state   : state Ms.t;
  initial : initial Ms.t;
}

val empty_eenv : eenv

val pp_t_ty      : Format.formatter -> t_ty -> unit
val pp_regions   : Format.formatter -> bvalue array Il.Mv.t -> unit
val pp_rdecl     : Format.formatter -> rdecl -> unit
val pp_vars      : Format.formatter -> value Il.Mv.t -> unit
val pp_state     : Format.formatter -> state -> unit
val pp_ival      : Format.formatter -> ival -> unit
val pp_initial   : Format.formatter -> initial -> unit
val pp_statevars : Format.formatter -> state * Scv.scvtarget -> unit

val find_state     : eenv -> Il.macro_name -> state
val update_state   : eenv -> Il.macro_name -> state -> eenv
val find_initial   : eenv -> Il.macro_name -> initial
val update_initial : eenv -> Il.macro_name -> initial -> eenv

val partial_eval : Il.genv -> eenv -> Il.macro -> state
