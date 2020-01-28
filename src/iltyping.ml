(* Copyright 2019-2020 - Inria, NXP *)

open Location
open Utils
open Common
open Il

let ty_error loc = error "type error" (loc, [])

let add_gvar genv x =
  try
    let x' = Ms.find x.v_name genv.glob_var in
    ty_error x.v_loc "global variable %s is already declared at %a"
      x.v_name pp_loc x'.v_loc
  with Not_found ->
    Glob_option.print_normal "iltyping: adding global %a@."
      Il.pp_global_g (Gvar x);
    { genv with glob_var = Ms.add x.v_name x genv.glob_var }

let add_macro genv m =
  try
    let m' = Ms.find m.mc_name genv.macro in
    ty_error m.mc_loc "the macro %s is already declared at %a"
      m.mc_name pp_loc m'.mc_loc
  with Not_found ->
    let f (p:Il.param) gls =
      match p with
      | Il.Pvar _ -> gls
      | Il.Plabel l -> Ml.add l m.mc_name gls
    in
    let glob_lbl = List.fold_right f m.mc_locals genv.glob_lbl in
    Glob_option.print_full "@[DEBUG add macro %S: labels:{@   @[<v>%a@]}@]@."
      m.mc_name (pp_glob_lbl ~full:true) glob_lbl;
    { genv with macro = Ms.add m.mc_name m genv.macro; glob_lbl }

let update_macro genv m =
  let f (p:Il.param) gls =
    match p with
    | Il.Pvar _ -> gls
    | Il.Plabel l -> Ml.add l m.mc_name gls
  in
  try
    let mold = Ms.find m.mc_name genv.macro in
    if mold.mc_uid = m.mc_uid then
      ty_error m.mc_loc "refusing to update macro with same uid" m
    else
      let glob_lbl = List.fold_right f m.mc_locals genv.glob_lbl in
      Glob_option.print_full "@[DEBUG update macro %s: labels:{@   @[<v>%a@]}@]@."
        m.mc_name (pp_glob_lbl ~full:true) glob_lbl;
      { genv with
        macro = Ms.update m.mc_name m.mc_name m genv.macro;
        glob_lbl;
      }
  with
    Not_found ->
    let glob_lbl : macro_name Ml.t = List.fold_right f m.mc_locals genv.glob_lbl in
    Glob_option.print_full "@[DEBUG update macro %s: labels:{@   @[<v>%a@]}@]@."
      m.mc_name (pp_glob_lbl ~full:true) glob_lbl;
    { genv with macro = Ms.add m.mc_name m genv.macro; glob_lbl}

let find_macro_loc genv m =
  let loc = loc m in
  let m = unloc m in
  try Ms.find m genv.macro
  with Not_found -> ty_error loc "unknown macro %s" m

let find_macro genv mname =
  try Ms.find mname genv.macro
  with Not_found -> ty_error Location._dummy "unknown macro %s" mname

let find_macro_opt genv mname =
  Ms.Exceptionless.find mname genv.macro

let get_macros genv =
  List.map snd (Ms.bindings genv.macro)

(* ************************************************* *)
(* Local environment                                 *)

type id_kind = [%import: Iltyping.id_kind]
type unsatdep = [%import: Iltyping.unsatdep]
type env = [%import: Iltyping.env]

let empty_env genv mn : env = {
  genv;
  locals = Ms.empty;
  unsat = Ms.empty;
  current = mn;
  currenti = None;
}

(* add a variable to the local definitions of this macro *)
let add_var (env:env) (x:var) : env =
  match Ms.find x.v_name env.locals with
  | Var x' ->
    ty_error x.v_loc "variable %s is already declared at %a"
       x.v_name pp_loc x'.v_loc
  | Label _ ->
    ty_error x.v_loc "variable %s already declared as a label" x.v_name
  | exception Not_found ->
    { env with locals = Ms.add x.v_name (Var x) env.locals }

(* add a local label to the header of this macro *)
let add_label (env:env) (local:bool) (l:Ilast.label) : env * Lbl.t =
  let ln = Ilast.label_to_string l in
  match Ms.find ln env.locals with
  | Var x' ->
    ty_error l.l_loc "label %a is already declared as a variable at %a"
      Ilast.pp_label l pp_loc x'.v_loc
  | Label _ ->
    ty_error l.l_loc "label %a is already declared" Ilast.pp_label l
  | exception Not_found ->
    (* check dependencies *)
    match Ms.find ln env.unsat with
    | DVar x', ms ->
      ty_error l.l_loc "label %a is already declared as variable at %a in %a"
        Ilast.pp_label l pp_loc x'.v_loc (pp_list ",@," pp_string) ms
    | DLabel l', ms ->
      (if not local then
         ty_error l.l_loc "non-unique label: label %a is already declared as %a in %a"
           Ilast.pp_label l Lbl.pp_g l' (pp_list ",@," pp_string) ms;
       (* dependency is now resolved (i.e. declared) *)
       { env with
         unsat = Ms.remove ln env.unsat;
         locals = Ms.add ln (Label(l',local)) env.locals }, l')
    | exception Not_found ->
      (* create a new label *)
      let l' = Lbl.fresh l.l_base l.l_offs in
      { env with locals = Ms.add ln (Label(l',local)) env.locals }, l'

(* find an existing local or global variable (not inside other macros) *)
let find_var env x =
  let loc = loc x in
  let x = unloc x in
  match Ms.find x env.locals with
  | Label(l,_) -> ty_error loc "%s is a label %a, not a variable" x Lbl.pp_g l
  | Var x   -> x
  | exception Not_found ->
    try Ms.find x env.genv.glob_var
    with Not_found ->
      (match Ms.find x env.unsat with
       | DLabel ld, ms ->
         ty_error loc "%s is not defined as variable but as label %a in dependency of %a"
           x Lbl.pp_dbg ld (pp_list ",@," pp_string) ms
       | DVar x, _   -> ty_error loc "unexpected"
       (* this makes no sense: variables need to be local or global,
                             but not shared between macros *)
       | exception Not_found ->
         ty_error loc "unknown variable %s" x)

(* find an existing label within other macros *)
let find_label_glob (env:env) (l:Ilast.label) : Lbl.t option=
  let ml = Ml.bindings env.genv.glob_lbl in
  match List.filter (fun (a,_) -> Lbl.compdata l.l_base l.l_offs a) ml with
  | [] ->
    None
  | [l,_] ->
    Some l
  | _ ->
    ty_error l.l_loc "labels must be unique but \
                  multiple labels named %a defined" Ilast.pp_label l

(* find existing local, global or unsatisfied label.
   otherwise create a fresh label
   make it a dependency if makedep is set *)
let find_label (makedep:bool) (env:env) (l:Ilast.label) : env * Lbl.t * bool =
  let ln = Ilast.label_to_string l in
  match Ms.find ln env.locals with
  | Label(l', local) ->
    { env with unsat = Ms.remove ln env.unsat }, l', local
  | Var _ ->
    ty_error l.l_loc "%a is typed as variable, not as label" Ilast.pp_label l
  | exception Not_found when String.equal env.current l.l_base ->
    ty_error l.l_loc "%a must be a local label but is not locally defined" Ilast.pp_label l
  | exception Not_found ->
    (* label is outside current macro *)
    match find_label_glob env l with
    | Some l -> env, l, false
    | None ->
      (* try to match an existing dependency *)
      match Ms.find ln env.unsat with
      | DLabel l', _ ->
        { env with unsat = Ms.remove ln env.unsat }, l', true
      | DVar x', ms ->
        ty_error l.l_loc "label %a is already declared as variable at %a in %a"
          Ilast.pp_label l pp_loc x'.v_loc (pp_list ",@," pp_string) ms
      | exception Not_found ->
        (* create new label and add it to the dependencies *)
        let l' = Lbl.fresh l.l_base l.l_offs in
        if makedep then
          { env with unsat = Ms.add ln (DLabel(l'), [env.current]) env.unsat }, l', false
        else
          env, l', false

(* try to find destination in a goto statement and mark it unsatisfied otherwise *)
let find_goto (env:env) (l:Ilast.label) : env * id_kind=
  let ln = Ilast.label_to_string l in
  (* try to find a local variable or label *)
  try env, Ms.find ln env.locals
  with Not_found ->
    (* try to find global variables *)
    try env, Var(Ms.find ln env.genv.glob_var)
    with Not_found ->
      (* try to find the label and create a dependency if not yet existing *)
      let env, l', b = find_label true env l in
      assert(b == false);
      (* define a dependency *)
      env, Label(l', false)

(* ********************************************** *)
(* Type checking                                  *)

let process_ty loc ty =
  match ty with
  | Tarr(_, i1, i2) when B.lt i2 i1 ->
    ty_error loc "%a should be less than %a" B.pp_print i1 B.pp_print i2
  |  _ -> ty

let check_ty_arr loc ty =
  match ty with
  | Tarr (bty,i1,i2) -> bty, i1, i2
  | _      ->
    ty_error loc "the expression has type %a while an array type is expected"
      pp_ty ty

let check_ty_mem loc ty =
  match ty with
  | Tmem -> ()
  | _ ->
    ty_error loc "the expression has type %a while a memory type is expected"
      pp_ty ty

let check_ty_ws loc ty =
  match ty with
  | Tbase (W ws) -> ws
  | _ ->
    ty_error loc "the expression has type %a while a word is expected"
      pp_ty ty

let check_ty_base loc ty =
  match ty with
  | Tbase bty -> bty
  | _ ->
    ty_error loc "the expression has type %a while a base type is expected"
      pp_ty ty

let check_type loc ety ty =
  if not (ty_eq ety ty) then
    ty_error loc "the expression has type %a instead of %a"
      pp_ty ety pp_ty ty

let process_var_decl vd =
  let loc = loc vd in
  let vd  = unloc vd in
  let ty  = process_ty loc vd.Ilast.v_type in
  V.fresh (unloc vd.Ilast.v_name) loc ty

let process_gvar_decl genv vd =
  let x = process_var_decl vd in
  add_gvar genv x

let get_op1 o =
  match o with
  | Ilast.OPP ws ->
    let ty = omap_dfl tw tint ws in
    Oopp ws, [ty], ty

  | Ilast.NOT ws ->
    let ty = omap_dfl tw tbool ws in
    Onot ws, [ty], ty

  | Ilast.SignExtend _ | Ilast.ZeroExtend _ | Ilast.Cast _  -> assert false

let ty2 o tdfl ws =
  let ty = omap_dfl tw tdfl ws in
  o ws, [ty; ty], ty

let ty2_b o tdfl ws =
  let ty = omap_dfl tw tdfl ws in
  o ws, [ty; ty], tbool

let ty2_ws loc o ws =
  let ws = ofdfl (fun _ -> ty_error loc "this operator except a size") ws in
  let ty = tw ws in
  o ws, [ty; ty], ty

let ty2_ws_i loc o ws =
  let ws = ofdfl (fun _ -> ty_error loc "this operator except a size") ws in
  let ty = tw ws in
  o ws, [ty; tint], ty

let get_op2 loc o ws =
  match o with
  | Ilast.ADD   -> ty2 (fun ws -> Oadd ws) tint ws
  | Ilast.SUB   -> ty2 (fun ws -> Osub ws) tint ws
  | Ilast.MUL   -> ty2 (fun ws -> Omul ws) tint ws
  | Ilast.MULH  -> ty2_ws loc (fun ws -> Omulh ws) ws
  | Ilast.LSL   -> ty2_ws_i loc (fun ws -> Olsl ws) ws
  | Ilast.LSR   -> ty2_ws_i loc (fun ws -> Olsr ws) ws
  | Ilast.ASR   -> ty2_ws_i loc (fun ws -> Oasr ws) ws
  | Ilast.AND   -> ty2 (fun ws -> Oand ws) tbool ws
  | Ilast.XOR   -> ty2 (fun ws -> Oxor ws) tbool ws
  | Ilast.OR    -> ty2 (fun ws -> Oor ws) tbool ws
  | Ilast.EQ    -> assert false
  | Ilast.NEQ   -> assert false
  | Ilast.NAMECMP -> assert false
  | Ilast.LT s  -> ty2_b (fun ws -> Olt(s,ws)) tint ws
  | Ilast.LE s  -> ty2_b (fun ws -> Ole(s,ws)) tint ws

let get_op o =
  match unloc o with
  | Ilast.Op1 o        -> get_op1 o
  | Ilast.Op2 (o', ws) -> get_op2 (loc o) o' ws
  | Ilast.Opn _        -> assert false

let get_e1 loc es =
  match es with
  | [e1] -> e1
  | _   -> ty_error loc "one argument is excepted"

let get_e2 loc es =
  match es with
  | [e1;e2] -> e1, e2
  | _   -> ty_error loc "two arguments are excepted"

let get_e3 loc es =
  match es with
  | [e1;e2;e3] -> e1, e2, e3
  | _   -> ty_error loc "three arguments are excepted"

let rec type_e env e =
  match unloc e with
  | Ilast.Eint i -> Eint i, tint
  | Ilast.Ebool b -> Ebool b, tbool
  | Ilast.Evar x ->
    let x = find_var env x in
    Evar x, x.v_ty
  | Ilast.Eget(x, e) ->
    let x, e, ty = type_arr_access env x e in
    Eget(x,e), ty
  | Ilast.Eload(ws,x,e) ->
    let x, e, ty = type_mem_access env ws x e in
    Eload(ws,x,e), ty
  | Eop(o,es) ->
    match unloc o with
    | Ilast.Op1 (Ilast.SignExtend ws) ->
      let loc = loc e in
      let e = get_e1 loc es in
      let e, ety = type_e env e in
      let wse = check_ty_ws loc ety in
      let ty = tw ws in
      if not (ws_le wse ws) then
        ty_error loc "the expression has type %a, but it should be less than %a"
          pp_ty (tw wse) pp_ty ty;
      let od = Osignextend(wse, ws) in
      let o = { od } in
      Eop(o, [e]), ty

    | Ilast.Op1 (Ilast.ZeroExtend ws) ->
      let loc = loc e in
      let e = get_e1 loc es in
      let e, ety = type_e env e in
      let wse = check_ty_ws loc ety in
      let ty = tw ws in
      if not (ws_le wse ws) then
        ty_error loc "the expression has type %a, but it should be less than %a"
          pp_ty (tw wse) pp_ty ty;
      let od = Ozeroextend(wse, ws) in
      let o = { od } in
      Eop(o, [e]), ty

    | Ilast.Op1 (Ilast.Cast (CW ws)) ->
      let loc = loc e in
      let e = get_e1 loc es in
      let e = check_e env e tint in
      let od = Ocast_w ws in
      let o = { od } in
      Eop(o, [e]), tw ws

    | Ilast.Op1 (Ilast.Cast (Cint s)) ->
      let loc = loc e in
      let e = get_e1 loc es in
      let e, ety = type_e env e in
      let bty = check_ty_base loc ety in
      let od = Ocast_int(s, bty) in
      let o = { od } in
      Eop(o, [e]), tint

    | Ilast.Op2 (Ilast.EQ, ws)   ->
      type_eq env (loc e) ws es

    | Ilast.Op2 (Ilast.NEQ, ws)  ->
      let e, ty = type_eq env (loc e) ws es in
      let od = Onot None in
      let o = { od } in
      Eop(o, [e]), ty

    | Ilast.Op2 (Ilast.NAMECMP, _) when
        (match env.currenti with
         | Some {pl_desc=Ilast.Iif _} -> true
         | _ -> false)  ->
      let e1, e2 = get_e2 (loc e) es in
      (match type_e env e1, type_e env e2 with
      | (Evar v1, bty1), (Evar v2, bty2) ->
        let _ = check_ty_base (loc e) bty1 in
        let _ = check_ty_base (loc e) bty2 in
        Eop({od=Onamecmp}, [Evar v1; Evar v2]), tbool
      | (e1, _), (e2, _) ->
        ty_error (loc e) "expected variables in operation but got %a"
          Ilast.pp_expr e)

    | Ilast.Op2 (Ilast.NAMECMP, _) ->
      ty_error (loc e) "operation %s not allowed outside if statement %a"
        (op_string { od = Il.Onamecmp}) Ilast.pp_expr e

    | Ilast.Opn "if" ->
      let loc = loc e in
      let e1, e2, e3 = get_e3 loc es in
      let e1 = check_e env e1 tbool in
      let e2, ty = type_e env e2 in
      let e3 = check_e env e3 ty in
      let od = Oif ty in
      let o = { od } in
      Eop(o,[e1;e2;e3]), ty

    | Ilast.Opn s ->
      ty_error (loc o) "unknown operator %s" s

    | _ ->
      let od, targs, tres = get_op o in
      let es = check_es env es targs in
      let o = { od } in
      Eop(o, es), tres

and type_arr_access env x e =
  let xl = loc x in
  let x = find_var env x in
  let e = check_e env e tint in
  let bty, _, _ = check_ty_arr xl x.v_ty in
  x, e, Tbase bty

and type_mem_access env ws x e =
  let xl = loc x in
  let x = find_var env x in
  let e = check_e env e tint in
  check_ty_mem xl x.v_ty;
  x, e, tw ws

and type_eq env loc1 ws es =
  let e1, e2 = get_e2 loc1 es in
  let loc1   = loc e1 in
  let e1, ty = type_e env e1 in
  begin match ws with
  | Some ws -> check_type loc1 ty (tw ws)
  | None    -> ()
  end;
  let e2     = check_e env e2 ty in
  let bty    = check_ty_base loc1 ty in
  let od     = Oeq bty in
  let o      = { od } in
  Eop(o, [e1; e2]), tbool

and check_e env e t =
  let loc = loc e in
  let e, et = type_e env e in
  check_type loc et t;
  e

and check_es env es ts =
  List.map2 (check_e env) es ts

let type_lval env lv =
  match lv with
  | Ilast.Lvar x ->
    let x = find_var env x in
    Lvar x, x.v_ty

  | Ilast.Lset (x, e) ->
    let x, e, ty = type_arr_access env x e in
    Lset(x,e), ty

  | Ilast.Lstore(ws,x,e) ->
    let x, e, ty = type_mem_access env ws x e in
    Lstore(ws,x,e), ty

let check_bound loc n1 n2 j1 j2 =
  if not (B.le n1 n2) then (* n2 < n1 *)
    ty_error loc "invalid range, %a should be smaller than %a"
      B.pp_print n1 B.pp_print n2;
  if not (B.le j1 n1) then (* n1 < j1 *)
    ty_error loc "invalid index, %a should be greater than %a"
      B.pp_print n1 B.pp_print j1;
  if not (B.le n2 j2) then (* j2 < n2*)
    ty_error loc "invalid index, %a should be smaller than %a"
      B.pp_print n1 B.pp_print j2

let check_arg env a p : env * Il.macro_arg =
  match p with
  | Plabel _ ->
    begin match a with
      | Ilast.Alabel lbl ->
        (* try to find the label and create a dependency if not yet existing *)
        let env, lbl, _ = find_label true env lbl in
        env, Alabel lbl
      | Ilast.Aexpr { pl_desc = Ilast.Evar lbl } ->
        let astlbl:Ilast.label =
          { l_base = unloc lbl; l_offs = B.zero; l_loc = loc lbl } in
        (* try to find the label and create a dependency if not yet existing *)
        let env, lbl, _ = find_label true env astlbl in
        env, Alabel lbl
      | Ilast.Aexpr {pl_loc = loc }
      | Ilast.Aindex ({pl_loc = loc}, _) ->
        ty_error loc "a label is expected"
    end
  | Pvar x ->
    match x.v_ty with
    | Tarr(bty, i1, i2) ->
      let xloc, x, bty', n1, n2 =
        match a with
        | Ilast.Aexpr e ->
          let loc = loc e in
          let e, ty = type_e env e in
          let x = destr_var e in
          let bty', n1, n2 = check_ty_arr loc ty in
          loc, x, bty', n1, n2
        | Ilast.Aindex(x,(n1,n2)) ->
          let loc = loc x in
          let x = find_var env x in
          let bty', j1, j2 = check_ty_arr loc x.v_ty in
          check_bound loc n1 n2 j1 j2;
          loc, x, bty', n1, n2
        | Ilast.Alabel l ->
          ty_error l.l_loc "the expression is a label %a instead of a %a"
            Ilast.pp_label l pp_bty bty
      in
      if not (ty_eq (Tbase bty) (Tbase bty')) then
        ty_error xloc "the expression is an array of %a instead of %a"
          pp_bty bty' pp_bty bty;
      let sn = B.sub n2 n1 in
      let si = B.sub i2 i1 in
      if not (B.equal sn si) then
        ty_error xloc "this array has size %a instead of %a"
          B.pp_print sn B.pp_print si;
      env, Aindex(x, n1, n2)
    | ty ->
      let e =
        match a with
        | Ilast.Aexpr e -> check_e env e ty
        | Ilast.Aindex (x,(n1,n2)) ->
          let x' = find_var env x in
          let bty, j1, j2 = check_ty_arr (loc x) x'.v_ty in
          check_type (loc x) (Tbase bty) ty;
          if not (B.equal n1 n2) then
            ty_error (loc x) "the expression is an array not a %a" pp_ty ty;
          check_bound (loc x) n1 n2 j1 j2;
          Eget(x', Eint n1)
        | Ilast.Alabel lbl ->
          ty_error lbl.l_loc "an expression is expected"
      in
      env, Aexpr e

let check_args env loc args params : env * macro_arg list =
  let len1 = List.length args in
  let len2 = List.length params in
  if len1 <> len2 then
    ty_error loc "invalid number of arguments %i provided instead of %i"
      len1 len2;
  let check_arg' (env,ps) a p : env * macro_arg list =
    let env, p' = check_arg env a p in
    env, p'::ps in
  let env, args = List.fold_left2 check_arg' (env,[]) args params in
  env, List.rev args

let rec type_i (env:env) (i:Ilast.instr) : env * Il.instr =
  let env = { env with currenti = Some i } in
  let env, i_desc =
    match unloc i with
    | Ilast.Iassgn(x,e) ->
      let x, ty = type_lval env x in
      let e = check_e env e ty in
      env, Iassgn(x,e)

    | Ilast.Ileak(i,es) ->
      let es = List.map (fun e -> fst (type_e env e)) es in
      env, Ileak(i,es)

    | Ilast.Imacro(mname,args) ->
      let m = find_macro_loc env.genv mname in
      let env, args = check_args env (loc i) args m.mc_params in
      env, Imacro(m.mc_name,args)

    | Ilast.Ilabel lbl ->
      let env, lbl', local = find_label false env lbl in
      if not local then
        ty_error lbl.l_loc
          "label definitions must be unique but %a is defined elsewhere"
          Ilast.pp_label lbl;
      env, Ilabel lbl'

    | Ilast.Igoto lbl ->
      begin match find_goto env lbl with
      | env, Label(lbl, _) -> env, Igoto lbl
      | env, Var x         -> env, Iigoto x
      end

    | Ilast.Iif(e,c1,c2) ->
      let e = check_e env e tbool in
      let env, c1 = type_c env c1 in
      let env, c2 = type_c env c2 in
      env, Iif(e,c1,c2)

    | Ilast.Iwhile(c1,e,c2) ->
      let e = check_e env e tbool in
      let env, c1 = type_c env c1 in
      let env, c2 = type_c env c2 in
      env, Iwhile(c1,e,c2) in
  env, { i_desc; i_loc = loc i, []; }

and type_c env c : env * Il.cmd =
  List.fold_left_map type_i env c

let process_param local env = function
  | Ilast.Pvar xd ->
    let xd = process_var_decl xd in
    add_var env xd, Pvar xd
  | Ilast.Plabel lbl ->
    let env, lbl = add_label env local lbl in
    env, Plabel lbl

let process_params local env ps =
  List.map_fold (process_param local) env ps

let process_macro env m : env * Il.macro =
  let loc    = loc m in
  let m      = unloc m in
  let mc_name = unloc m.Ilast.mc_name in
  let env     = { env with current = mc_name } in

  let params = m.Ilast.mc_params in
  let locals = m.Ilast.mc_locals in
  let body   = m.Ilast.mc_body in

  let env, mc_params = process_params false env params in
  let env, mc_locals = process_params true  env locals in
  let env, mc_body   = type_c env body in

  let m = {
      mc_name;
      mc_uid  = Uid.fresh ();
      mc_loc  = loc;
      mc_params;
      mc_locals;
      mc_body } in
  env, m

let process_macros genv (ms:Ilast.macro_decl located list) =
  let env = empty_env genv "" in
  let env, ms = List.fold_left_map process_macro env ms in
  let ms = List.rev ms in
  (* check dependencies *)
  if not (Ms.is_empty env.unsat) then
     ty_error _dummy "unsatisfied dependencies:@   @[<v>%a@]"
       (fun fmt ms -> Ms.iter (fun k v ->
            let pp_dep fmt (d,ms) =
              match d with
              | DLabel l -> Format.fprintf fmt "missing label %a in %a"
                              Lbl.pp_g l (pp_list ",@," pp_string) ms
              | DVar v -> Format.fprintf fmt "missing var %a in %a"
                            V.pp_g v (pp_list ",@," pp_string) ms
            in
            Format.fprintf fmt "@[%s -> %a;@]@ " k pp_dep v)
            ms)
       env.unsat;
  (* update genv *)
  let genv' = List.fold_left update_macro genv ms in
  (* debug print all the macros *)
  if !Glob_option.full then
    List.iter (fun m -> Format.printf "@[<v>type checked %s@ %a@]@."
                  m.mc_name (Il.pp_global ~full:true) (Gmacro m)) ms;
  (* check labels *)
  List.iter (Il.check_labels "type error in check_labels" genv') ms;
  genv'

let rec check_initval (env:env) (loc:Location.t) (v:Ilast.initval) (ty:Common.ty)
  : Ileval.ival =
  let open Ileval in
  match v with
  | Ilast.Iptr(x, ofs) ->
    let x = find_var env x in
    let _, i1, i2 = check_ty_arr loc x.v_ty in
    if not ((B.le i1 ofs) && (B.le ofs i2)) then
      Glob_option.eprint_normal
        "initial value of pointer [%a %a] is out of range %a[%a:%a] (%a)"
        V.pp_g x B.pp_print ofs V.pp_g x B.pp_print i1 B.pp_print i2
        Location.pp_loc loc;
      Ileval.Iptr(x, ofs)
  | Ilast.Ibool b ->
    check_type loc tbool ty;
    Ileval.Ibool b
  | Ilast.Iint  i ->
    check_type loc tint ty;
    Ileval.Iint i
  | Ilast.Iexit   ->
    Ileval.Icptr_exit
  | Ilast.Ilbl l ->
    (match find_label_glob env l with
    | Some l' -> Ileval.Ilbl l'
    | None ->
      ty_error loc "unknown label %a as initial value" Ilast.pp_label l)
  | Ilast.Iword(ws, iv) ->
    check_type loc (tw ws) ty;
    Ileval.Iint iv
  | Ilast.Iarr ivs ->
    let bty, i1, i2 = get_arr ty in
    let ielems = List.length ivs in
    let eelems = B.to_int (B.succ (B.sub i2 i1)) in
    if (eelems == ielems) then
      Ileval.Iarr (List.map (fun iv -> check_initval env loc iv (Tbase bty)) ivs)
    else
      ty_error loc "initial value has %d elements but expected %d values" ielems eelems

let process_annotation genv evi =
  let open Ileval in
  let mn = evi.Ilast.eval_m in
  let m = find_macro_loc genv mn in
  let ir = ref [] in
  let iv = ref [] in
  let ipv = ref [] in
  let opv = ref [] in
  let ivd = ref [] in
  let env0 = empty_env genv (unloc mn) in
  let env = ref env0 in
  let convty ty =
    match ty with
    | Ilast.Sharing -> Ileval.Sharing
    | Ilast.URandom -> Ileval.URandom
    | Ilast.Public  -> Ileval.Public
    | Ilast.Secret  -> Ileval.Secret in
  let process_io_annot ty x orng =
    let lx = loc x in
    let x = find_var !env x in
    let ity = convty ty in
    match ity, orng with
    | _, Some(i1, i2) ->
      let _, j1, j2 = check_ty_arr lx x.v_ty in
      check_bound lx i1 i2 j1 j2;
      (ity, x)
    | _, None ->
      (ity, x)
  in

  let process_dest ws d = 
    match d with
    | Ilast.REvar x ->
      let loc = loc x in
      let x = find_var !env x in
      check_type loc x.v_ty (tw ws);
      [{ d_var = x; d_ofs = None }]
    | Ilast.REget (x, i) ->
      let loc = loc x in
      let x = find_var !env x in
      let bty,i1,i2 = check_ty_arr loc x.v_ty in
      check_type loc (Tbase bty) (tw ws);
      check_bound loc i i i1 i2;
      [{ d_var = x; d_ofs = Some i }]
    | Ilast.REblock(x, (i,j)) ->
        let loc = loc x in
      let x = find_var !env x in
      let bty,i1,i2 = check_ty_arr loc x.v_ty in
      check_type loc (Tbase bty) (tw ws);
      check_bound loc i j i1 i2;
      let size = B.to_int (B.add (B.sub j i) B.one) in
      List.init size (fun k -> {d_var = x; d_ofs = Some (B.add i (B.of_int k))})
  in

  let process_dests loc d dest = 
    let ws,i1,i2 = get_arr d.v_ty in
    let ws = check_ty_ws loc (Tbase ws) in
    let size = B.to_int (B.add (B.sub i2 i1) B.one) in
    match dest with
    | None -> 
      Array.init size 
        (fun i -> {d_var = d; d_ofs = Some (B.add i1 (B.of_int i))})
    | Some ds ->
      let t = Array.of_list (List.flatten (List.map (process_dest ws) ds)) in
      if Array.length t <> size then
        ty_error loc 
          "bad region declaration, %i elements are declared and %i are provided"
          size (Array.length t);
      t in
    
  let process_ii = function
    | Ilast.Var vd ->
      let d = process_var_decl vd in
      env := add_var !env d;
      ivd := d :: !ivd
    | Ilast.Region (mem, ws, dest, (i1,i2), decls) ->
      let dloc = loc dest in
      let mloc = loc mem in
      let mem = find_var env0 mem in
      check_ty_mem mloc mem.v_ty;
      let vd =
        mk_loc (loc dest)
          { Ilast.v_name = dest; Ilast.v_type = Tarr(W ws,i1,i2) } in
      let d = process_var_decl vd in
      let dest = process_dests dloc d decls in
      env := add_var !env d;
      ir := { r_from = mem; r_name = d; r_dest = dest } :: !ir
    | Ilast.Init (x, v) ->
      let loc = loc x in
      let x = find_var !env x in
      let ty = x.v_ty in
      let v = check_initval !env loc v ty in
      iv := (x,v) :: !iv
    | Ilast.Input(ty, x, orng) ->
      let (ty, x) = process_io_annot ty x orng in
      ipv := (ty, x) :: !ipv
    | Ilast.Output(ty, x, orng) ->
      let (ty, x) = process_io_annot ty x orng in
      opv := (ty, x) :: !opv in
  List.iter process_ii evi.eval_i;
  m, { init_vard = List.rev !ivd;
       init_region = List.rev !ir;
       init_var    = List.rev !iv;
       input_var   = List.rev !ipv;
       output_var  = List.rev !opv;}

let macronames_of_scvtarget genv t =
  match t with
  | Scv.TIdent [] -> []
  | Scv.TIdent l ->
    let unfound = List.filter
        (fun n ->
           match Ms.Exceptionless.find (unloc n) genv.macro with
           | Some _ -> false
           | None -> true)
        l
    in
    if List.length unfound == 0 then
      List.map unloc l
    else
      Utils.hierror "Iltyping.macronames_of_scvtarget" (Some (loc (List.hd unfound)))
        "environment contains no definition of @[<v>%a@]"
        (pp_list ",@," Scv.pp_scvstring) unfound
  | Scv.TWildcard _ ->
    List.map fst (Ms.bindings genv.macro)
  | Scv.TRegex r ->
    let regex = Re.execp (Re.compile (Re.Glob.glob (unloc r))) in
    Ms.fold (fun k _ ms -> if regex k then k::ms else ms) genv.macro []
