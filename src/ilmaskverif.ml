(* Copyright 2019-2020 - NXP, Inria *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)
open Utils
open Common
module MV = Maskverif
module MVP = Maskverif.Prog
module MVE = Maskverif.Expr
module MVU = Maskverif.Util

module IlToMv(* : sig
  val to_maskverif: Il.macro -> Ileval.initial -> Ileval.state -> MVP.func
  val get_or_lift: Il.macro -> Ileval.initial -> Ileval.state -> MVP.func
  val get_mvprog: Il.macro -> MVP.func

  val lift_illoc: Location.t -> MVU.location

end *) = struct
  module MTP = Maskverif.Prog.ToProg
  module P = Maskverif.Parsetree

  module Mf = Map.Make(Il.M)

  type ilvmapping =
    | MScalar of MVE.var
    | MArray of MVE.var array

  type mvvmapping =
    | IlVar of Il.var
    | IlArr of Il.var * int
    | IlLeakname of string * Location.t

  (* global lookups *)

  (* lookup from Il.macro -> MVP.func *)
  let globilmacro2func : MVP.func Mf.t ref = ref Mf.empty

  (* maskverif internal env *)
  let mvglobalenv : MVP.global_env = Hashtbl.create 107

  type mvkind =
    | Local
    | Pin
    | Inp
    | Rand

  type liftstate =
    {
      names      : unit Ms.t;
      mvkind     : mvkind MVE.Mv.t;
      il2mv      : ilvmapping Il.Mv.t; (* translate il variable to mv correspondence *)
      mv2il      : mvvmapping MVE.Mv.t; (* translate back an mv variable to its il origin *)
    }

  let error details = Utils.hierror "Lift from Il to Maskverif" details

  let lift_illoc (l:Location.t) : MVU.location =
    {
      lc_fname = l.loc_fname;
      lc_start = l.loc_start;
      lc_end   = l.loc_end;
      lc_bchar = l.loc_bchar;
      lc_echar = l.loc_echar;
    }

  let lift_ws (ws:Common.wsize) : MVE.ty =
    match ws with
    | Common.U8 -> MVE.W8
    | Common.U16 -> MVE.W16
    | Common.U32 -> MVE.W32
    | Common.U64 -> MVE.W64

  let lift_bty (l:Location.t option) (bty:Common.bty) : MVE.ty =
    match bty with
    | Common.Bool -> MVE.W1
    | Common.W ws -> lift_ws ws
    | Common.Int -> MVE.int

  let lift_ilty (v:Il.var) : MVE.ty =
    match v.v_ty with
    | Common.Tbase(bty) -> lift_bty (Some v.v_loc) bty
    | Common.Tarr(bty,_,_) -> lift_bty (Some v.v_loc) bty
    | Common.Tmem ->
      error (Some v.v_loc)
        "@[Maskverif does not support memory, eliminate %a prior analysis.@]@."
        Il.V.pp_dbg v

  let mk_name env v =
    let s = v.Il.v_name in
    if Ms.mem s env.names then
      let s = Format.sprintf "%s/%a" s Uid.pp_s v.v_id in
      if (Ms.mem s env.names) then
        error None
          "@[rewritten name %s already exists.@]@." s;
      s
    else
      s

  let mk_name_i env v i =
    let s = Format.sprintf "%s[%i]" v.Il.v_name i in
    if Ms.mem s env.names then
      let s = Format.sprintf "%s/%a[%i]" v.v_name Uid.pp_s v.v_id i in
      assert (not (Ms.mem s env.names));
      s
    else s

  let add_bvar env v =
    match Il.Mv.find v env.il2mv with
    | MScalar x -> env, x
    | MArray _ -> assert false
    | exception Not_found ->
      let name = mk_name env v in
      let x = MVE.V.mk_var name (lift_ilty v) in
      { env with
        names = Ms.add name () env.names;
        il2mv = Il.Mv.add v (MScalar x) env.il2mv;
        mv2il = MVE.Mv.add x (IlVar v) env.mv2il }, x

  let add_avar env v =
    match Il.Mv.find v env.il2mv with
    | MScalar _ -> assert false
    | MArray xs -> env, xs
    | exception Not_found ->
      let bty, i1, i2 = Common.get_arr v.Il.v_ty in
      let env = ref env in
      let create i =
        let name = mk_name_i !env v i in
        let x = MVE.V.mk_var name (lift_bty (Some v.v_loc) bty) in
        env := {!env with
                 names = Ms.add name () !env.names;
                 mv2il = MVE.Mv.add x (IlArr(v, i)) !env.mv2il };
        x in
      let size = B.to_int (B.sub i2 i1) + 1 in
      let xs = Array.init size create in
      let env = { !env with
                  il2mv = Il.Mv.add v (MArray xs) !env.il2mv; } in
      env, xs

  let pp_mvkind fmt = function
    | Local -> Format.fprintf fmt "local"
    | Pin   -> Format.fprintf fmt "public input"
    | Inp   -> Format.fprintf fmt "input"
    | Rand  -> Format.fprintf fmt "random"


  let add_kind k env x =
    try
      let k' = MVE.Mv.find x env.mvkind in
      error None "duplicate declaration of %s, already declared as %a"
        x.MVE.v_name pp_mvkind k'
    with Not_found ->
      { env with mvkind = MVE.Mv.add x k env.mvkind }

  let add_pin = add_kind Pin
  let add_in  = add_kind Inp
  let add_rnd = add_kind Rand
  let add_local = add_kind Local

  let add_pout env x = env
  let add_out env x = env

  open Ilmaskverifoperators

  let check_init for_decl loc env x =
    if not (MVE.Mv.mem x !env.mvkind) then
      if for_decl then env := add_local !env x
      else
        error (Some loc) "use a variable before initialisation @."


  let lift_var ~for_decl env v =
    match Il.Mv.find v !env.il2mv with
    | MScalar x ->
      check_init for_decl v.v_loc env x;
      x
    | MArray _ -> assert false
    | exception Not_found ->
      if for_decl then
        let env0, x = add_bvar !env v in
        env := add_local env0 x;
        x
      else
        error (Some v.v_loc) "use a variable before initialisation"

  let get_index_i ty i = 
    let _, i1, _ = get_arr ty in
    B.to_int (B.sub i i1)

  let get_index ty = function
    | Il.Eint i -> get_index_i ty i 
    | _ -> assert false

  let lift_avar ~for_decl env v i =
    let i = get_index v.Il.v_ty i in
    match Il.Mv.find v !env.il2mv with
    | MScalar x -> assert false
    | MArray xs ->
      let x = xs.(i) in
      check_init for_decl v.v_loc env x;
      x
    | exception Not_found ->
      if for_decl then
        let env0, xs = add_avar !env v in
        let x = xs.(i) in
        env := add_local env0 x;
        x
      else
        error (Some v.v_loc) "use a variable before initialisation"



  let rec lift_expr (i:Il.instr) (env:liftstate ref) (expr:Il.expr) =
    match expr with
    | Il.Ebool true ->
      MVP.Econst MVE.C._true
    | Il.Ebool false ->
      MVP.Econst MVE.C._false
    | Il.Eint bi ->
      MVP.Econst (MVE.C.make MVE.INT (Common.B.to_zint bi))
    | Il.Evar v ->
      MVP.Evar (lift_var ~for_decl:false env v)
    | Il.Eget(v, i) ->
      MVP.Evar(lift_avar ~for_decl:false env v i)
    | Il.Eop(op, es) -> lift_op i env op es
    | Il.Eload(_,_,_) ->
      error (Some (fst i.i_loc))
        "@[cannot lift memory %a. perform partial evaluation first.@]@."
        Il.pp_e_dbg expr

  and lift_op (i:Il.instr) (env:liftstate ref) (op:Il.op) (es:Il.expr list) =
    let err_unsupported () =
      error (Some (fst i.i_loc))
        "@[op %a not supported.@]@."
        Il.pp_e_dbg (Il.Eop(op,es)) in
    let err_invalid () =
      error (Some (fst i.i_loc))
        "@[op %a invalid, perform partial evaluation.@]@."
        Il.pp_e_dbg (Il.Eop(op,es)) in

    let mves = List.map (lift_expr i env) es in

    let lift_existing (op:MVE.operator) =
      match mves with
      | [e] ->
        MVP.Eop1(op, e)
      | [e1;e2] ->
        MVP.Eop2(op, e1, e2)
      | _ ->
        MVP.Eop(op, mves)
    in
    let lift_or (oand:MVE.operator) (onot:MVE.operator) =
      match mves with
      | [e1;e2] ->
        (* or a b = not (and (not a) (not b)) *)
        MVP.Eop1(onot, MVP.Eop2(oand, MVP.Eop1(onot, e1), MVP.Eop1(onot,e2)))

      | _ ->
        error (Some (fst i.i_loc)) "@[unexpected or in %a.@]@."
          Il.pp_e_dbg (Il.Eop(op,es)) in

    match op.od with
    | Il.Oor t                  -> lift_or (o_and t) (o_not t)
    | Il.Oxor t                 -> lift_existing (o_xor t)
    | Il.Oand t                 -> lift_existing (o_and t)
    | Il.Onot t                 -> lift_existing (o_not t)
    | Il.Oadd t                 -> lift_existing (o_add t)
    | Il.Osub t                 -> lift_existing (o_sub t)
    | Il.Omul t                 -> lift_existing (o_mul t)
    | Il.Oopp t                 -> lift_existing (o_opp t)
    | Il.Ocast_int(Signed, t)   -> lift_existing (o_castsint t)
    | Il.Ocast_int(Unsigned, t) -> lift_existing (o_castuint t)
    | Il.Olt(Signed, t)         -> lift_existing (o_lts t)
    | Il.Olt(Unsigned, t)       -> lift_existing (o_ltu t)
    | Il.Ole(Signed, t)         -> lift_existing (o_les t)
    | Il.Ole(Unsigned, t)       -> lift_existing (o_leu t)
    | Il.Olsl t                 -> lift_existing (o_lsl t)
    | Il.Olsr t                 -> lift_existing (o_lsr t)
    | Il.Oasr t                 -> lift_existing (o_asr t)
    | Il.Ocast_w t              -> lift_existing (o_cast_w t)
    | Il.Oeq t                  -> lift_existing (o_eq t)
    | Il.Osignextend(ws1, ws2)
    | Il.Ozeroextend(ws1, ws2)  -> err_unsupported ()
    | Il.Onamecmp | Il.Oif _    -> err_invalid ()
    | Il.Omulh(ws)              -> err_unsupported ()

  let lift_Iassgn (i:Il.instr) (env: liftstate ref) (lvar:Il.lval) (rhs:Il.expr)
    : MVP.instr =
    let i_var =
      match lvar with
      | Il.Lvar v -> lift_var ~for_decl:true env v
      | Il.Lset(v,i) -> lift_avar ~for_decl:true env v i
      | _ -> Format.eprintf "%a@." Il.pp_i_g i; assert false
    in
    let i_kind = MV.Parsetree.IK_noleak in
    let i_expr = lift_expr i env rhs in
    let instr_d = MVP.Iassgn({i_var; i_kind; i_expr}) in
    (* TODO improve location *)
    let instr_info = MVP.ToProg.pp_loc_info (lift_illoc (fst i.i_loc)) "" in
    { instr_d; instr_info }

  let lift_Ileak i env (li:Il.leak_info) (es:Il.expr list) =
    let lname, s =
      match li with
      | Some lname -> lname, lname
      | None -> "unnamedleak", "_" in

    let l_name = MVE.V.mk_var lname MVE.w1 in
    env := {!env with
             mv2il =
               MVE.Mv.add l_name (IlLeakname(lname, fst i.Il.i_loc))
                 !env.mv2il};
    let lis =
      Format.asprintf "@[leak %s(%a) at %s@]"
        s (Utils.pp_list ", " Il.pp_e_g) es
        (List.fold_right
           (fun l s -> String.concat "\n" [Location.tostring l; s])
           (snd i.i_loc) "") in
    let es = List.map (lift_expr i env) es in

    let l_exprs = MVP.Eop(MVE.o_tuple, es) in
    let instr_d = MVP.Ileak({l_name; l_exprs}) in
    let instr_info = MVP.ToProg.pp_loc_info (lift_illoc (fst i.i_loc)) lis in
    { MVP.instr_d; instr_info }

  let lift_instr env body =
    let body =
      List.filter (* remove the labels to keep lift_instr neat and clean *)
        (function { Il.i_desc = Il.Ilabel _} -> false | _ -> true) body in

    let lift_i env i =
      match i.Il.i_desc with
      | Il.Iassgn (lvar, expr) ->
        lift_Iassgn i env lvar expr
      | Il.Ileak (li, es) ->
        lift_Ileak i env li es
      | Il.Imacro (_, _) ->
        error (Some (fst i.i_loc))
          "@[macro calls not yet supported: cannot handle %a@]@."
          Il.pp_i_dbg i
      | Il.Ilabel _ -> assert false
      | Il.Igoto _
      | Il.Iigoto _
      | Il.Iif (_, _, _)
      | Il.Iwhile (_, _, _) ->
        error (Some (fst i.i_loc))
          "@[Expecting evaluated program, cannot handle %a@]@."
          Il.pp_i_dbg i
    in
    let env = ref env in
    let body = List.map (lift_i env) body in
    body, !env


  let atys_of_an aty ans =
    List.filter (fun (aty',_, _) -> aty' = aty) ans

  let add_rd env = function
    | Ileval.RDvar x ->
      add_bvar env x
    | Ileval.RDget (x, i) ->
      let env, xs = add_avar env x in
      let i = get_index_i x.Il.v_ty i in
      env, xs.(i)

  let init_header_vars aty env ans =
    let vars = atys_of_an aty ans in
    List.map_fold (fun env (_,x,rds) ->
        let env, xs = List.map_fold add_rd env (Array.to_list rds) in
        env, (x,xs)) env vars

  let init_header add aty env ans =
    let env, xs = init_header_vars aty env ans in
    let env, xs =
      List.map_fold (fun env (_, xs) ->
          List.fold_left add env xs, xs) env xs in
    List.flatten xs, env

  let init_pin  env ans = init_header add_pin Ileval.Public env ans
  let init_rnd  env ans = init_header add_rnd Ileval.URandom env ans
  let init_pout env ans = init_header add_pout Ileval.Public env ans

  let init_in env ans =
    let env, xs = init_header_vars Ileval.Sharing env ans in
    let env, xs =
      List.map_fold (fun env (v,xs) ->
        let env = List.fold_left add_in env xs in
        let name = mk_name env v in
        let x = MVE.V.mk_var name (lift_ilty v) in
        let env =
          { env with
            names = Ms.add name () env.names; } in
        env, (x,xs)) env xs in
   xs, env

  let init_out env ans =
    let env, xs = init_header_vars Ileval.Sharing env ans in
    let env, xs =
      List.map_fold (fun env (_,xs) ->
        let env = List.fold_left add_out env xs in
        env, xs) env xs in
    xs, env

  let to_maskverif (m:Il.macro) (an:Ileval.initial) (st:Ileval.state)
    : MVP.func =
    if Mf.mem m !globilmacro2func then
      error None
        "@[macro %s already lifted.@]@." m.mc_name;
    let lenv : liftstate = {
        names  = Ms.empty;
        mvkind = MVE.Mv.empty;
        il2mv = Il.Mv.empty;
        mv2il = MVE.Mv.empty;
      } in
    let f_name = MVU.HS.make m.mc_name in
    let f_kind = MVU.NONE in
    let f_pin, lenv = init_pin lenv an.input_var in
    let f_rand, lenv = init_rnd lenv an.input_var in
    let f_pout, lenv = init_pout lenv an.output_var in
    let f_ein = [] in
    let f_in, lenv = init_in lenv an.input_var in
    let f_out, lenv = init_out lenv an.output_var in
    (* body *)

    let f_cmd, lenv = lift_instr lenv st.st_eprog in
    let f_other =
      MVE.Mv.fold (fun x k l ->
          if k = Local then x::l else l) lenv.mvkind [] in
    let (func:MVP.func) = {
      f_name; (* function name *)
      f_pin;  (* public input *)
      f_ein;  (* for tight private circuits/tightPROVE), not used by scVerif *)
      f_kind; (* SNI gadget or not, scVerif does not use it *)
      f_in;   (* input shares *)
      f_out;  (* output shares *)
      f_pout; (* public outputs which are checked to be probabilistic independent *)
      f_other;(* internal variables *)
      f_rand; (* input entropy *)
      f_cmd } in
    (* as in Maskverif.Prog.func *)
    let func = MVP.Process.macro_expand_func mvglobalenv func in
    (* update the global state accordingly *)
    (* update the binding *)
    globilmacro2func := Mf.add m func !globilmacro2func;
    (* return the resulting func *)
    func

  let get_mvprog (m:Il.macro) : MVP.func =
    try Mf.find m !globilmacro2func
    with Not_found ->
      error None
        "@[cannot find lift of macro %s@]@." m.mc_name

  let get_or_lift (m:Il.macro) (an:Ileval.initial) (st:Ileval.state) =
    try Mf.find m !globilmacro2func
    with Not_found ->
      to_maskverif m an st

end

let print_mvprog (m:Il.macro) (an:Ileval.initial) (st:Ileval.state) =
  let func = IlToMv.get_or_lift m an st in
  let pi:MVP.print_info = {var_full = !Glob_option.full; print_info = false} in
  Format.printf "@[<v>%a@]@."
    (MVP.pp_func ~full:pi) func

let check_mvprog (params:Scv.scvcheckkind) (m:Il.macro) (an:Ileval.initial) (st:Ileval.state)
  : unit =
  let func = IlToMv.get_or_lift m an st in
  (* debug print the program *)
  Glob_option.print_full "Lifted maskverif program:@     @[<v>%a@]@."
    (MVP.pp_func ~full:MVP.dft_pinfo) func;
  let algorithm =
    (match params with
    | Scv.StatefulNoninterference
    | Scv.Noninterference -> MVU.(`NI)
    | Scv.StatefulStrongnoninterference
    | Scv.Strongnoninterference -> MVU.(`SNI)
    )
  in
  let (mvparams, nb_shares, interns, outputs, pubout, _) =
    MVP.build_obs_func
      ~ni:algorithm ~trans:false ~glitch:false
      (IlToMv.lift_illoc m.mc_loc) func in
  let order = (nb_shares - 1) in
  let toolopts:MVU.tool_opt = {
    pp_error = true;
    checkbool = true;
  } in
  let success = match params with
  | Scv.StatefulNoninterference ->
    MV.Checker.check_ni
      toolopts ~para:true ~fname:(m.mc_name) mvparams nb_shares ~order
      ~outpub:pubout interns
  | Scv.StatefulStrongnoninterference ->
    MV.Checker.check_sni
      toolopts ~para:true ~fname:(m.mc_name) mvparams nb_shares ~order
      interns ~outpub:pubout outputs
  | Scv.Noninterference ->
    MV.Checker.check_ni
      toolopts ~para:true ~fname:(m.mc_name) mvparams nb_shares ~order interns
  | Scv.Strongnoninterference ->
    MV.Checker.check_sni
      toolopts ~para:true ~fname:(m.mc_name) mvparams nb_shares ~order
      interns outputs in
  if success then
    Format.printf "successful check@."
  else
    Format.printf "failed check@."
