open Utils
open Location
open Common

open Asmast
open Asmparser
open Asmlexer

open Ilast

let lift_reg (r:Asmast.ident) =
  let loc = loc r in
  let ident = unloc r in
  let (expr:Ilast.expr) = mk_loc loc (Ilast.Evar (mk_loc loc ident)) in
  [Ilast.Aexpr(expr)]

let lift_imm (i:B.zint located) =
  let loc = loc i in
  let ival = unloc i in
  let expr = mk_loc loc (Ilast.Eint ival) in
  [Ilast.Aexpr(expr)]

let rec lift_labels (l:Asmast.label list) =
  match l with
  | NLabel i :: ls ->
    let loc = loc i in
    let ident = unloc i in
    let label = mk_loc loc ident in
    [Ilast.Alabel label] @ lift_labels ls
  | Hex h :: ls ->
    let loc = loc h in
    let ival = unloc h in
    let label = mk_loc loc (B.to_string ival) in
    [Ilast.Alabel label] @ lift_labels ls
  | [] -> []

let rec lift_regoffs (r:Asmast.ident) (o:Asmast.operand) =
  (lift_reg r) @ (lift_operands [o])

and lift_operands (ops:Asmast.operand list) =
  match ops with
  | Reg r :: xs -> (lift_reg r) @ lift_operands xs
  | Imm i :: xs -> (lift_imm i) @ lift_operands xs
  | RegOffs(r, o) :: xs -> (lift_regoffs r o) @ lift_operands xs
  | Label ll :: xs -> (lift_labels ll) @ lift_operands xs
  | [] -> []

let lift_stmt (secname:string) (stmt:Asmast.stmt) =
  let stmt_loc = loc stmt in
  let stmt = unloc stmt in
  let mname = mk_loc _dummy stmt.instr_asm in
  let (margs:Ilast.macro_arg list) = lift_operands stmt.instr_exp in
  let idesc = Ilast.Imacro (mname, margs) in
  let lbl_loc = loc stmt.offset in
  let lbl = (secname ^ "+" ^ (B.to_string (unloc stmt.offset))) in
  let lbl = mk_loc stmt_loc (Ilast.Ilabel (mk_loc lbl_loc lbl)) in
  let ins = mk_loc stmt_loc idesc in
  let (cmd:Ilast.cmd) = [lbl; ins] in
  cmd

let lift_section (sec:Asmast.section) =
  (* section becomes a macro with call statements *)
  let sloc = loc sec in
  let m = unloc sec in
  let mname = m.s_name in
  let secname = unloc mname in
  let (mcalls:Ilast.cmd) = List.flatten (List.map (lift_stmt secname) m.s_stmts) in

  let (m:Ilast.macro_decl) = {
    mc_name   = mname;
    mc_params = []; (* FIXME : requires additional user input *)
    mc_locals = []; (* FIXME : initial state requires additional user input*)
    mc_body   = mcalls
  } in
  Ilast.Gmacro (mk_loc sloc m)
