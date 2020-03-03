(* Copyright 2019 - NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear *)

val serialize_mvcheck: Scv.scvcheckkind -> Ileval.state -> Ileval.initial -> Il.macro -> unit
val serialize_mvprog: Ileval.state -> Ileval.initial -> Il.macro -> unit
