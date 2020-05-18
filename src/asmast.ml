(* Copyright 2019-2020 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

open Location
open Common

type t = [%import: Location.t]
[@@deriving show]

type 'a located = [%import: 'a Location.located]
[@@deriving show]

type ident  = string located [@@deriving show]
type hex    = B.zint located [@@deriving show]

(** operands of assembly instructions *)
type operand =
  | Reg     of ident                          (** register by name *)
  | Imm     of B.zint located                 (** constant number *)
  | Bool    of bool located                   (** rarely used flag *)
  | Label   of ident * B.zint located option  (** label with optional integrated offset *)
[@@deriving show]

type stmt_r = {
  offset    : hex;
  instr_bin : hex;
  instr_asm : ident;
  instr_exp : operand list
} [@@deriving show]

type stmt = stmt_r located [@@deriving show]

type section_r = {
  s_adr    : hex;
  s_name   : ident;
  s_stmts  : stmt list
} [@@deriving show]

type section = section_r located [@@deriving show]
