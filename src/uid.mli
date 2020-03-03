(* Copyright 2019 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear *)

type t 
val fresh : unit -> t
val compare : t -> t -> int
val equal   : t -> t -> bool
val pp : Format.formatter -> t -> unit
val pp_s : unit -> t -> string
