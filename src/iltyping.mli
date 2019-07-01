open Utils
open Common
open Il
type genv

val empty_genv : genv

val process_var_decl  : Ilast.var_decl Location.located -> V.t
val add_gvar          : genv -> V.t -> genv


val process_macro : genv -> Ilast.macro_decl Location.located -> macro
val add_macro     : genv -> macro -> genv
val update_macro  : genv -> macro -> genv
val find_macro_opt: genv -> string -> macro option

val process_eval  : genv -> Ilast.eval_info -> Il.macro * Ileval.initial
val process_apply : genv -> Ilast.apply_info -> Il.macro list
