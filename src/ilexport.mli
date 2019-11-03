(* Copyright 2019 - NXP *)

val print_mv: Scv.scvcheckkind -> Ileval.state -> Ileval.initial -> Il.macro -> unit
val check_maskverif: Scv.scvcheckkind -> Ileval.state -> Ileval.initial -> Il.macro -> bool
