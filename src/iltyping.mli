open Utils
open Common
open Il

type genv = {
  glob_var : V.t Ms.t;
  macro    : macro Ms.t;
}

type id_kind =
  | Label of Lbl.t * bool (* true means local label *)
  | Var   of var

type env = {
  genv   : genv;
  locals : id_kind Ms.t;
}

val empty_genv : genv

val process_var_decl : Ilast.var_decl Location.located -> V.t
val add_gvar         : genv -> V.t -> genv

val process_macro  : genv -> Ilast.macro_decl Location.located -> macro
val add_macro      : genv -> macro -> genv
val update_macro   : genv -> macro -> genv
val find_macro_opt : genv -> string -> macro option
val find_macro     : genv -> string -> macro

val process_annotation : genv -> Ilast.eval_info -> Il.macro * Ileval.initial
val process_apply_ms   : genv -> Ilast.apply_info -> Il.macro list
val process_apply_m    : genv -> Ilast.apply_info -> Il.macro
val process_apply_ls   : genv -> Ilast.apply_info -> Ilast.ident list
