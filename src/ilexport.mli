(* Copyright 2019 - NXP *)

val serialize_mvcheck: Scv.scvcheckkind -> Ileval.state -> Ileval.initial -> Il.macro -> unit
val serialize_mvprog: Ileval.state -> Ileval.initial -> Il.macro -> unit
