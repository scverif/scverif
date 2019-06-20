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
  let name = secname ^ "+" ^ (B.to_string_X offs) in
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
  let margs = lift_operands stmt.instr_exp in
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
