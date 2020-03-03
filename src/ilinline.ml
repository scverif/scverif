(* Copyright 2019 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear *)

open Utils
open Common
open Il

let in_error loc = error "inline" loc

let var_index loc x y =
  in_error loc "variable %a is the index %a" V.pp_dbg x V.pp_dbg  y

type vinfo =
  | VIexpr  of expr
  | VIindex of V.t * B.zint * B.zint

type env = {
    mutable evar    : vinfo Mv.t;
    mutable elbl    : Lbl.t Ml.t;
    mutable elocals : param list;
    mutable econt   : cmd
  }

let empty_env elocals econt = {
    evar   = Mv.empty;
    elbl   = Ml.empty;
    elocals;
    econt
  }

let add_param env p arg =
  match p, arg with
  | Pvar x, Aexpr e ->
    env.evar <- Mv.add x (VIexpr e) env.evar
  | Pvar x, Aindex(y,i1,i2) ->
    env.evar <- Mv.add x (VIindex(y,i1,i2)) env.evar
  | Plabel lbl1, Alabel lbl2 ->
    env.elbl <- Ml.add lbl1 lbl2 env.elbl
  | _, _ -> assert false

let add_params env ps args =
  List.iter2 (add_param env) ps args

let add_dparam env p =
  match p with
  | Pvar x ->
    let y = V.clone x in
    env.evar <- Mv.add x (VIexpr (Evar y)) env.evar;
    Pvar y
  | Plabel lbl ->
    let lbl' = Lbl.clone lbl in
    env.elbl <- Ml.add lbl lbl' env.elbl;
    Plabel lbl'

let add_dparams env ps =
  List.map (add_dparam env) ps

let add_local env p =
  let p' = add_dparam env p in
  env.elocals <- p' :: env.elocals

let add_locals env ps =
  List.iter (add_local env) ps

let find_var env loc x =
  try Mv.find x env.evar
  (* Globals are not bound *)
  with Not_found -> VIexpr (Evar x)
  (* FIXME: ensure that x is global *)
  (* in_error loc "unbound variable %a, please report" V.pp_dbg x *)

let find_lbl (genvlookup:bool) (g:genv) (e:env) (loc:Utils.full_loc) (lbl:Lbl.t) =
  (* first try to find a local label *)
  try Ml.find lbl e.elbl
  with Not_found ->
    if genvlookup then
      if Ml.mem lbl g.glob_lbl then
        lbl
      else
        in_error loc "%a@ local and global unbound label %a, cannot substitute"
          (pp_genv ~full:true) g Lbl.pp_dbg lbl
    else
      in_error loc "locally unbound label %a, cannot substitute" Lbl.pp_dbg lbl

let get_var loc e =
  match e with
  | Evar x -> x
  | _ -> in_error loc "the expression %a is not a variable" (pp_e ~full:true) e

let rec inline_e env loc e =
  match e with
  | Eint _ | Ebool _ -> e
  | Evar x -> inline_var env loc x
  | Eget(x, e) -> inline_get env loc x e
  | Eload(ws, x, e) -> inline_load env loc ws x e
  | Eop(op, es) -> Eop(op, inline_es env loc es)

and inline_es env loc es =
  List.map (inline_e env loc) es

and inline_var env loc x =
  match find_var env loc x with
  | VIexpr e -> e
  | VIindex(y,_,_) -> var_index loc x y

and inline_get env loc x e =
  let e = inline_e env loc e in
  let _, n1, n2 = get_arr x.v_ty in
  let y, i1, i2 =
    match find_var env loc x with
    | VIexpr e ->
      let y = get_var loc e in
      let _, i1, i2 = get_arr x.v_ty in
      y, i1, i2
    | VIindex (y, i1, i2) -> y, i1, i2 in
  assert (check_size n1 n2 i1 i2);
  let e = addi_imm (subi_imm e n1) i1 in
  Eget(y, e)

and inline_load env loc ws x e =
  let e = inline_e env loc e in
  let m =
    match find_var env loc x with
    | VIexpr e -> get_var loc e
    | VIindex (y, _, _) -> var_index loc x y in
  Eload(ws,m,e)

let inline_lv env loc x =
  let e = inline_e env loc (lv2e x) in
  try e2lv e
  with Not_found ->
    in_error loc "   @[<v>lvalue %a inline to %a which cannot be viewed as lvalue@]"
      (pp_lval ~full:true) x pp_e_dbg e

let inline_arg genv env loc arg =
  match arg with
  | Aexpr e -> Aexpr (inline_e env loc e)
  | Alabel lbl ->
    Alabel (find_lbl true genv env loc lbl)
  | Aindex(x, j1, j2) ->
    let _, n1, n2 = get_arr x.v_ty in
    let y, i1, i2 =
      match find_var env loc x with
      | VIexpr e ->
        let y = get_var loc e in
        let _, i1, i2 = get_arr x.v_ty in
        y, i1, i2
      | VIindex (y, i1, i2) -> y, i1, i2 in
   assert (check_size n1 n2 i1 i2);
   (* [n1 .. j1 .. j2 .. n2]
      [i1 .............. i2] *)
   Aindex(y, B.add i1 (B.sub j1 n1), B.add i1 (B.sub j2 n1))

let inline_args genv env loc args =
  List.map (inline_arg genv env loc) args

let add_i env loc id =
  let i = { i_desc = id; i_loc = loc } in
  env.econt <- i :: env.econt

let rec inline_i genv env locs i =
  let loc = append_locs i.i_loc locs in
  match i.i_desc with
  | Iassgn(x,e) ->
    add_i env loc (Iassgn(inline_lv env loc x, inline_e env loc e))
  | Ileak(i,es) ->
    add_i env loc (Ileak(i, inline_es env loc es))
  | Imacro(m,args) ->
    let args = inline_args genv env loc args in
    let locs = fst loc :: snd loc in
    inline_macro_app genv env locs m args
  | Ilabel lbl ->
    (* only local labels are allowed *)
    let lbl = find_lbl false genv env loc lbl in
    add_i env loc (Ilabel lbl)
  | Igoto lbl ->
    (* global jumps allowed, not inlined *)
    let lbl = find_lbl true genv env loc lbl in
    add_i env loc (Igoto lbl)
  | Iigoto x ->
    (* global jumps allowed, not inlined *)
    add_i env loc (Iigoto (get_var loc (inline_var env loc x)))
  | Iif(e,c1,c2) ->
    let e  = inline_e env loc e in
    let c1 = inline_c genv env locs c1 in
    let c2 = inline_c genv env locs c2 in
    add_i env loc (Iif(e,c1,c2))
  | Iwhile(c1,e,c2) ->
    let c1 = inline_c genv env locs c1 in
    let e  = inline_e env loc e in
    let c2 = inline_c genv env locs c2 in
    add_i env loc (Iwhile(c1,e,c2))

and inline_c genv env locs c =
  let cont = env.econt in
  env.econt <- [];
  List.iter (inline_i genv env locs) c;
  let c = List.rev env.econt in
  env.econt <- cont;
  c

and inline_macro_app genv env locs mcname args =
  let envm = empty_env env.elocals env.econt in
  let m = Iltyping.find_macro genv mcname in
  add_params envm m.mc_params args;
  add_locals envm m.mc_locals;
  List.iter (inline_i genv envm locs) m.mc_body;
  env.elocals <- envm.elocals;
  env.econt  <- envm.econt

let inline_macro (genv:genv) (m:Il.macro) =
  let env = empty_env [] [] in
  let mc_params = add_dparams env m.mc_params in
  add_locals env m.mc_locals;
  List.iter (inline_i genv env []) m.mc_body;
  { mc_name = m.mc_name;
      mc_uid = Uid.fresh ();
      mc_loc = m.mc_loc;
      mc_params;
      mc_locals = env.elocals;
      mc_body = List.rev env.econt;
    }
