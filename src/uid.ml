(* Copyright 2019 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear *)

type t = int

let count = ref (-1)

let fresh () = incr count; !count

let compare i1 i2 = i1 - i2
let equal i1 i2 = i1 == i2

let pp fmt i = Format.fprintf fmt "%i" i

let pp_s () i = Format.sprintf "%i" i
