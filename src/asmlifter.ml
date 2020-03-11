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

let refine_label (name:string) (base:B.zint) (offs:Asmast.hex) : Ilast.label =
  let loc = loc offs in
  let offs = unloc offs in
  (* offset is relative to base (e.g. start of macro)*)
  let offset = (B.sub offs base) in
  { l_base = name; l_offs = offset; l_loc = loc }

let lift_regimm = function
  | Reg r -> Ilast.mk_var r
  | Imm i -> Ilast.mk_cast_w U32 (mk_int i)

let aexpr_var id = Aexpr (Ilast.mk_var id)

let lift_operand o =
  match o with
  | Regimm ir       -> [Aexpr (lift_regimm ir)]
  | RegOffs (r, ir) -> [aexpr_var r; Aexpr (lift_regimm ir)]
  | Label(id,offs)  -> [Alabel (refine_label (unloc id) B.zero offs)]

let lift_operands = function
  | Ofixed ops -> List.flatten (List.map lift_operand ops)
  | Oflexible rs -> List.map aexpr_var rs
  | Onone -> []

let lift_stmt (secname:string) (baseaddr:B.zint) (stmt:Asmast.stmt) =
  let stmt_loc = loc stmt in
  let stmt  = unloc stmt in
  let margs = lift_operands stmt.instr_exp in
  let mname = refine_mname stmt.instr_asm (List.length margs) in
  let lbl   = refine_label secname baseaddr stmt.offset in
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
  let secname = unloc mc_name in
  let base = unloc m.s_adr in
  (* compute the body *)
  let mc_body = List.flatten (List.map (lift_stmt secname base) m.s_stmts) in
  let mc_locals = infer_param_labels mc_body in
  let m = { mc_name; mc_params = []; mc_locals; mc_body } in
  mk_loc sloc m
