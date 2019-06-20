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

let refine_mname (asmstmt:Asmast.stmt) = 
  let loc = loc asmstmt in
  let asmstmt = unloc asmstmt in
  let mname = clear_name asmstmt.instr_asm in
  let mname = 
    match asmstmt.instr_exp with 
    | Ofixed    _    -> mname 
    | Oflexible args -> mname ^ (string_of_int (List.length args)) in
  mk_loc loc mname 

let refine_label (secname:string) (offs:Asmast.hex) = 
  let loc = loc offs in
  let offs = unloc offs in
  let name = secname ^ "+" ^ (B.to_string offs) in
  mk_loc loc name 

let lift_regimm = function
  | Reg r -> Ilast.mk_var r 
  | Imm i -> Ilast.mk_cast_w U32 (mk_int i)

let aexpr_var id = Aexpr (Ilast.mk_var id) 

let lift_operand o = 
  match o with
  | Regimm ir -> [Aexpr (lift_regimm ir)]
  | RegOffs (r, ir) -> [aexpr_var r; Aexpr (lift_regimm ir)]
  | Label(id,offs)   -> [aexpr_var (refine_label (unloc id) offs)]
  
let lift_operands = function
  | Ofixed ops -> List.flatten (List.map lift_operand ops)
  | Oflexible rs -> List.map aexpr_var rs 

let lift_stmt (secname:string) (stmt:Asmast.stmt) =
  let stmt_loc = loc stmt in
  let mname = refine_mname stmt in
  let stmt  = unloc stmt in
  let margs  = lift_operands stmt.instr_exp in
  let lbl   = refine_label secname stmt.offset in
  let ilbl  = mk_loc (loc lbl) (Ilast.Ilabel lbl) in
  let ins   = mk_loc stmt_loc  (Ilast.Imacro (mname, margs)) in
  [ilbl; ins] 

let infer_param_labels is = 
  let p = ref [] in
  let aux i = 
    match unloc i with
    | Ilabel lbl -> p := Plabel lbl :: !p 
    | _          -> () in
  List.iter aux is;
  !p

let lift_section (sec:Asmast.section) =
  (* section becomes a macro with call statements *)
  let sloc    = loc sec in
  let m       = unloc sec in
  let mc_name   = m.s_name in
  let secname = unloc mc_name in
  (* compute the body *)
  let mc_body = List.flatten (List.map (lift_stmt secname) m.s_stmts) in
  let mc_locals = infer_param_labels mc_body in
  let m = { mc_name; mc_params = []; mc_locals; mc_body } in
  Ilast.Gmacro (mk_loc sloc m)

(*

  

  let (mcalls:Ilast.cmd) = List.flatten (List.map (lift_stmt secname) m.s_stmts) in
  let (mparams:Ilast.param list) = infer_param_labels mcalls in

  let (m:Ilast.macro_decl) = {
    mc_name   = mname;
    mc_params = []; (* FIXME : initial state requires additional user input*)
    mc_locals = mparams;
    mc_body   = mcalls
  } in
  Ilast.Gmacro (mk_loc sloc m)




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
  let mname =
    if String.contains mname '.' then
      String.concat "" (String.split_on_char '.' mname)
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
 *)
