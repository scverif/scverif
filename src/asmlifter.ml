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

let lift_label (secname:string) (l:Asmast.label) =
  match l with
  | LSymbol(i, o) ->
    let loc = loc i in
    let ident = unloc i in
    let oval = unloc o in
    let label = mk_loc loc (ident ^ "+" ^ (B.to_string oval)) in
    label
  | LAddress h ->
    let loc = loc h in
    let ival = unloc h in
    (* BEWARE: THIS IS WRONG BUT WE DO NOT HAVE A NOTION OF ADDRESSES RIGHT NOW*)
    let label = mk_loc loc (secname ^ "+" ^ (B.to_string ival)) in
    label

let lift_labels (secname:string) (ll:Asmast.label list) =
  let (lll:Ilast.label list) = List.map (lift_label secname) ll in
  let ineq_lbl l1 l2 = (unloc l1) != (unloc l2) in
  let remove_duplicates l ls = if List.exists (ineq_lbl l) ls then ls else l :: ls in
  let (uls:Ilast.label list) = List.fold_right remove_duplicates lll [] in
  let (uals:Ilast.macro_arg list) = List.map (fun x -> Ilast.Alabel x) uls in
  uals

let rec lift_regoffs (secname:string) (r:Asmast.ident) (o:Asmast.operand) =
  (lift_reg r) @ (lift_operands secname [o])

and lift_operands (secname:string) (ops:Asmast.operand list) =
  match ops with
  | Reg r :: xs -> (lift_reg r) @ lift_operands secname xs
  | Imm i :: xs -> (lift_imm i) @ lift_operands secname xs
  | RegOffs(r, o) :: xs -> (lift_regoffs secname r o) @ lift_operands secname xs
  | Label ll :: xs -> (lift_labels secname ll) @ lift_operands secname xs
  | [] -> []

let refine_mname (secname:string) (asmstmt:Asmast.stmt_r) (margs:Ilast.macro_arg list) =
  let opcount = List.length margs in
  let mname =
    if opcount > 0 then
      asmstmt.instr_asm ^ (string_of_int opcount)
    else
      asmstmt.instr_asm
  in
  let pimm arg =
    match arg with
    | Alabel _
    | Aindex _ -> false
    | Aexpr e ->
      match unloc e with
      | Eint _ -> true
      | _ -> false in
  let mname =
    if List.exists pimm margs then
      mname ^ "_i"
    else
      mname
  in
  mname

let lift_stmt (secname:string) (stmt:Asmast.stmt) =
  let stmt_loc = loc stmt in
  let stmt = unloc stmt in
  let (margs:Ilast.macro_arg list) = lift_operands secname stmt.instr_exp in
  let mname = refine_mname secname stmt margs in
  let mname = mk_loc stmt_loc mname in
  let idesc = Ilast.Imacro (mname, margs) in
  let lbl_loc = loc stmt.offset in
  let lbl = (secname ^ "+" ^ (B.to_string (unloc stmt.offset))) in
  let lbl = mk_loc stmt_loc (Ilast.Ilabel (mk_loc lbl_loc lbl)) in
  let ins = mk_loc stmt_loc idesc in
  let (cmd:Ilast.cmd) = [lbl; ins] in
  cmd

let infer_param_labels (cmds:Ilast.cmd) =
  let is_lbl_decl (instr:Ilast.instr_desc) =
    match instr with
    | Ilast.Ilabel _ -> true
    | _ -> false in
  let param_from_cmd (ins:Ilast.instr) =
    match unloc ins with
    | Ilast.Ilabel i -> Ilast.Plabel i
    | _ -> assert false in
  let lbl_decls = List.filter (fun x -> is_lbl_decl (unloc x)) cmds in
  let (lbl_params:Ilast.param list) = List.map param_from_cmd lbl_decls in
  lbl_params

let lift_section (sec:Asmast.section) =
  (* section becomes a macro with call statements *)
  let sloc = loc sec in
  let m = unloc sec in
  let mname = m.s_name in
  let secname = unloc mname in
  let (mcalls:Ilast.cmd) = List.flatten (List.map (lift_stmt secname) m.s_stmts) in
  let (mparams:Ilast.param list) = infer_param_labels mcalls in

  let (m:Ilast.macro_decl) = {
    mc_name   = mname;
    mc_params = []; (* FIXME : initial state requires additional user input*)
    mc_locals = mparams;
    mc_body   = mcalls
  } in
  Ilast.Gmacro (mk_loc sloc m)
