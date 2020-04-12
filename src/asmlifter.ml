(* Copyright 2019 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

open Utils
open Location
open Common

open Asmast
open Asmparser
open Asmlexer

open Ilast

let clear_name (mname:string) =
  if String.contains mname '.' then
    String.concat "" (String.split_on_char '.' mname)
  else
    mname

let refine_mname (mname:ident) (nargs:int) =
  let loc = loc mname in
  let mname = clear_name (unloc mname) in
  let mname = mname ^ (string_of_int nargs) in
  mk_loc loc mname

let refine_label (name:ident) (base:B.zint) (offs:B.zint)
  : Ilast.label =
  (* offset is relative to base (e.g. start of macro)*)
  let offset = (B.sub offs base) in
  { l_base = unloc name; l_offs = offset; l_loc = loc name }

let mk_aexpr id = Aexpr (Ilast.mk_var id)

let lift_operand o =
  match o with
  | Reg n -> Aexpr (Ilast.mk_var n)
  | Bool b -> Aexpr (mk_loc (loc b) (Ebool (unloc b)))
  | Imm i -> Aexpr (Ilast.mk_cast_w U32 (mk_int i))
  | Label(n, Some ofs) -> Alabel (refine_label n B.zero (unloc ofs))
  | Label(n, None)     -> Alabel (refine_label n B.zero B.zero)

let lift_stmt (secname:ident) (baseaddr:B.zint) (stmt:Asmast.stmt) =
  let stmt_loc = loc stmt in
  let stmt  = unloc stmt in
  let margs = List.map lift_operand stmt.instr_exp in
  let mname = refine_mname stmt.instr_asm (List.length margs) in
  let lbl   = refine_label secname baseaddr (unloc stmt.offset) in
  let ilbl  = mk_loc (lbl.l_loc) (Ilast.Ilabel lbl) in
  let ins   = mk_loc stmt_loc  (Ilast.Imacro (mname, margs)) in
  [ilbl; ins]

let infer_param_labels is =
  let aux i ps =
    match unloc i with
    | Ilabel lbl -> Plabel lbl :: ps
    | _          -> ps in
  List.fold_right aux is []

let lift_section (sec:Asmast.section) =
  (* section becomes a macro with call statements *)
  let sloc    = loc sec in
  let m       = unloc sec in
  let mc_name = m.s_name in
  let base = unloc m.s_adr in
  (* compute the body *)
  let mc_body = List.flatten (List.map (lift_stmt mc_name base) m.s_stmts) in
  let mc_locals = infer_param_labels mc_body in
  let m = { mc_name; mc_params = []; mc_locals; mc_body } in
  mk_loc sloc m
