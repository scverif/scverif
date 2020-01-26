(* Copyright 2019-2020 - Inria, NXP *)

open Utils
open Common
open Il

(** Type-Checking transformations and operations for untrusted Il-AST **)

(** kind of local variable declaration *)
type id_kind =
  | Var   of var                              (** variables given as parameter *)
  | Label of Lbl.t * bool                     (** true means local label *)

(** record of (yet) undefined labels and variables within other macros *)
type unsatdep =
  | DLabel of Lbl.t                           (** label e.g. for jumps *)
  | DVar of V.t                               (** data attached to some macro *)

(** environment holding data for type-checking an Ilast macro *)
type env = {
  genv   : genv;                              (** read/write copy of global environment *)
  locals : id_kind Ms.t;                      (** local variables of assessed macro *)
  unsat  : (unsatdep * macro_name list) Ms.t; (** unsatisfied dependency with reference to their origin *)
  current : macro_name;                       (** identifier of macro under investigation *)
  currenti : Ilast.instr option;              (** instruction currently under investigation *)
}

(** type-check and lift multiple Ilast macro(s) to core Il macro(s).
   Returns global env with new macros added.  Allows cross
   dependencies such as global jumps to be satisfied out-of-order.
   Does not permit dependencies to global variables or labels which
   are not satisfied after type-checking the full input list.  *)
val process_macros     : genv -> Ilast.macro_decl Location.located list -> genv

(** lift declaration of global variable and return updated global env *)
val process_gvar_decl  : genv -> Ilast.var_decl Location.located -> genv

(** type-check a set of annotations for a specific macro *)
val process_annotation : genv -> Ilast.eval_info -> Il.macro * Ileval.initial

(** access to global env *)
val add_macro      : genv -> Il.macro -> genv
val update_macro   : genv -> Il.macro -> genv
val find_macro     : genv -> string -> Il.macro
val find_macro_opt : genv -> string -> Il.macro option

(** find matching macro names of scv target specification *)
val macronames_of_scvtarget  : genv -> Scv.scvtarget -> Il.macro_name list
