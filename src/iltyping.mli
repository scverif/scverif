(* Copyright 2019 - Inria, NXP *)

open Utils
open Common
open Il

type id_kind =
  | Label of Lbl.t * bool (* true means local label *)
  | Var   of var          (* variables given as parameters *)

(* keep track of (yet) undefined labels and variables within other macros *)
type unsatdep =
  | DLabel of Lbl.t (* label e.g. for jumps *)
  | DVar of V.t     (* data attached to some macro *)

type env = {
  genv   : genv;
  locals : id_kind Ms.t;
  (* unsatisfied dependency with reference to their origin *)
  (* TODO make use of macro_name list *)
  unsat  : (unsatdep * macro_name list) Ms.t;
  current : macro_name;
}

val process_var_decl : Ilast.var_decl Location.located -> V.t
val add_gvar         : genv -> V.t -> genv

val process_macros : genv -> Ilast.macro_decl Location.located list -> genv
val add_macro      : genv -> macro -> genv
val update_macro   : genv -> macro -> genv
val find_macro_opt : genv -> string -> macro option
val find_macro     : genv -> string -> macro

val process_annotation   : genv -> Ilast.eval_info -> Il.macro * Ileval.initial
val macronames_of_scvtarget  : genv -> Scv.scvtarget -> Il.macro_name list
