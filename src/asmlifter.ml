open Utils
open Location

open Asmast
open Asmparser
open Asmlexer

open Il

let lift_operands (ops:Asmast.operand list) =
  []

let lift_stmt (stmt:Asmast.stmt) =
  let stmt_loc = loc stmt in
  let stmt = unloc stmt in
  let mname = mk_loc _dummy stmt.instr_asm in
  let margs = lift_operands stmt.instr_exp in
  let idesc = Ilast.Imacro (mname, margs) in
  let ins = mk_loc stmt_loc idesc in
  ins

let lift_section (sec:Asmast.section) =
  (* section becomes a macro with call statements *)
  let sloc = loc sec in
  let m = unloc sec in
  let mname = m.s_name in
  let (mcalls:Ilast.cmd) = List.map lift_stmt m.s_stmts in

  let (m:Ilast.macro_decl) = {
    mc_name   = mname;
    mc_params = []; (* FIXME : requires additional user input *)
    mc_locals = []; (* FIXME : initial state requires additional user input*)
    mc_body   = mcalls
  } in
  Ilast.Gmacro (mk_loc sloc m)

let lift (ast:section) =
  let ilast = ([], []) in
  ilast
