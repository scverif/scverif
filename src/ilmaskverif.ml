(* Copyright 2019 - NXP *)

module MV = Maskverif
module MVP = Maskverif.Prog
module MVE = Maskverif.Expr
module MVU = Maskverif.Util

let mvglob = Hashtbl.create 107

module IlToMv = struct
  module MTP = Maskverif.Prog.ToProg
  module P = Maskverif.Parsetree

  let il2mv : MVE.var Il.Mv.t ref = ref Il.Mv.empty
  let mv2il : Il.var MVE.Mv.t ref = ref MVE.Mv.empty
  let mvglobenv = Hashtbl.create 107

  type liftstate =
    {
      globals    : MV.Prog.global_env;
      headerdefs : MVE.Sv.t;
      localdefs  : MVE.Sv.t;
      il2mv      : MVE.var list Il.Mv.t;
      mv2il      : Il.var MVE.Mv.t;
    }

  let empty_liftstate : liftstate =
    {
      globals = mvglobenv; (* local copy, has to be merged explicitly *)
      headerdefs = MVE.Sv.empty;
      localdefs = MVE.Sv.empty;
      il2mv = Il.Mv.empty;
      mv2il = MVE.Mv.empty;
    }

  let map_ilvar (v:Il.var) : MVE.var =
    Il.Mv.find v !il2mv

  let map_mvvar (v:MVE.var) : Il.var =
    MVE.Mv.find v !mv2il

  let error details = Utils.hierror "Lift from Il to Maskverif" details

  let lift_illoc (l:Location.t) : MVU.location =
    {
      lc_fname = l.loc_fname;
      lc_start = l.loc_start;
      lc_end   = l.loc_end;
      lc_bchar = l.loc_bchar;
      lc_echar = l.loc_echar;
    }

  let lift_ilvname (v:Il.var) : P.ident =
    MV.Util.mkloc (lift_illoc v.v_loc) v.v_name

  let lift_ilty (v:Il.var) : MVE.ty =
    let lift_bty (bty:Common.bty) : MVE.ty =
      match bty with
      | Common.Bool -> MV.Expr.W1
      | Common.W (Common.U8) -> MV.Expr.W8
      | Common.W (Common.U16) -> MV.Expr.W16
      | Common.W (Common.U32) -> MV.Expr.W32
      | Common.W (Common.U64) -> MV.Expr.W64
      | Common.Int ->
        error (Some v.v_loc)
          "@[Maskverif does not support variables with type Int@]@."
    in
    match v.v_ty with
    | Common.Tbase(bty) -> lift_bty bty
    | Common.Tarr(bty,_,_) -> lift_bty bty
    | Common.Tmem ->
      error (Some v.v_loc)
        "@[Maskverif does not support memory, eliminate %a prior analysis.@]@."
        Il.V.pp_g v

  (* translate il variable to maskverif var
   * keep the mapping in il2mv and mv2il *)
  let lift_ilvbase (env:liftstate) (v:Il.var) : MVE.var * liftstate =
    match v.v_ty with
    | Common.Tbase(bty) ->
      begin
        (* check if variable has already been lifted *)
        if (Il.Mv.mem v env.il2mv) then
          error (Some (v.v_loc))
            "@[variable %a has already been lifted.@]@."
            Il.V.pp_g v;

        (* make sure the variable's name has not been defined in the header elsewhere *)
        if (MVE.Sv.exists (fun var -> String.equal var.MVE.v_name v.v_name) env.headerdefs) then
          error (Some (v.v_loc))
            "@[variable %a defined multiple times in the header.@]@."
            Il.V.pp_g v;

        (* create the variable with unique id *)
        let mvvar = MVE.V.mk_var v.v_name (lift_ilty v) in
        (* add the variable to the set of defined variables *)
        let headerdefs = MVE.Sv.add mvvar env.headerdefs in
        (* update the bindings in our mapping *)
        let il2mv = Il.Mv.add v [mvvar] env.il2mv in
        let mv2il = MVE.Mv.add mvvar v env.mv2il in
        (* return the variable and the new env *)
        mvvar, {env with headerdefs; il2mv; mv2il}
      end
    | Common.Tarr(_,_,_) ->
      error (Some v.v_loc)
        "@[use lift_ilvarr instead]"
    | Common.Tmem ->
      error (Some v.v_loc)
        "@[Maskverif does not support memory, eliminate %a prior analysis.@]@."
        Il.V.pp_g v

  (* translate il array to maskverif var
   * keep the mapping in il2mv and mv2il *)
  let lift_ilvarr (liftbasevar:bool) (env:liftstate) (v:Il.var) : MVE.var list * liftstate =
    match v.v_ty with
    | Common.Tarr(bty,rs,re) ->
      begin
        let range : int list =
          let rec mk_range (s:int) (e:int) =
            if s > e then []
            else s :: mk_range (s + 1) e in
          mk_range (Common.B.to_int rs) (Common.B.to_int re) in
        let lift_index (i:int) (vs,env:MVE.var list * liftstate)
          : MVE.var list * liftstate =
          let vname = v.v_name ^ string_of_int i in
          (* check if variable has already been lifted *)
          if (Il.Mv.mem v env.il2mv) then
            error (Some (v.v_loc))
              "@[variable %a has already been lifted.@]@."
              Il.V.pp_g v;

          (* make sure the variable's name has not been defined in the header elsewhere *)
          if (MVE.Sv.exists (fun var -> String.equal var.MVE.v_name vname) env.headerdefs) then
            error (Some (v.v_loc))
              "@[variable %a defined multiple times in the header.@]@."
              Il.V.pp_g v;

          (* create the variable with unique id *)
          let mvvar = MVE.V.mk_var vname (lift_ilty v) in
          (* add the variable to the set of defined variables *)
          let headerdefs = MVE.Sv.add mvvar env.headerdefs in
          (* update the bindings in our mapping *)
          let mv2il = MVE.Mv.add mvvar v env.mv2il in
          (* return the variable and the new env *)
          mvvar::vs, {env with headerdefs; mv2il}
        in
        (* lift the individual indices *)
        let mvvars, env = List.fold_right lift_index range ([],env) in
        (* check wether we need to lift the base var as well *)
        if not liftbasevar then
          (* update the binding from il -> mv variable *)
          let il2mv = Il.Mv.add v mvvars env.il2mv in
          mvvars, {env with il2mv}
        else
          (* lift the base variable and return it at the front of the list *)
          begin
            (* make sure the variable's name has not been defined in the header elsewhere *)
            if (MVE.Sv.exists
                  (fun var -> String.equal var.MVE.v_name v.v_name)
                  env.headerdefs) then
              error (Some (v.v_loc))
                "@[variable %a defined multiple times in the header.@]@."
                Il.V.pp_g v;
            let mvbvar = MVE.V.mk_var v.v_name (lift_ilty v) in
            (* add the variable to the set of defined variables *)
            let headerdefs = MVE.Sv.add mvbvar env.headerdefs in
            (* update the bindings in our mapping *)
            let mv2il = MVE.Mv.add mvbvar v env.mv2il in
            (* return the variable and the new env *)
            let mvvars = mvbvar :: mvvars in
            (* update the binding from il -> mv variable *)
            let il2mv = Il.Mv.add v mvvars env.il2mv in
            mvvars, {env with il2mv; headerdefs; mv2il}
          end
      end
    | Common.Tbase(_) ->
      error (Some v.v_loc)
        "@[use lift_ilvbase instead]"
    | Common.Tmem ->
      error (Some v.v_loc)
        "@[Maskverif does not support memory, eliminate %a prior analysis.@]@."
        Il.V.pp_g v

  let atys_of_an (aty:Ileval.t_ty) (ans:(Ileval.t_ty * Il.var) list)
    : Il.var list =
    let filter ty (t,v) ls =
      if ty == t then
        v::ls
      else
        ls in
    List.fold_right (filter aty) ans []

  let init_header_vars (aty:Ileval.t_ty) (env:liftstate) (ans:(Ileval.t_ty * Il.var) list)
    : MVE.var list * liftstate =
    (* variables which are annotated correctly *)
    let vars = atys_of_an aty ans in
    let init_var (v:Il.var) (vs,env:MVE.var list * liftstate)
      : MVE.var list * liftstate =
      match v.v_ty with
      | Common.Tbase(_) ->
        let v', env = lift_ilvbase env v in
        v'::vs, env
      | Common.Tarr(bty,rs,re) ->
        (* need to lift every index as variable *)
        let vs', env = lift_ilvarr false env v in
        vs'@vs, env
      | Common.Tmem ->
        error (Some v.v_loc)
          "@[Maskverif does not support memory, eliminate %a prior analysis.@]"
          Il.V.pp_g v
    in
    (* initialize/define each random input variable, return the new leakage state*)
    List.fold_right init_var vars ([], env)

  let init_header_sharedinvars (aty:Ileval.t_ty) (env:liftstate) (ans:(Ileval.t_ty * Il.var) list)
    : ((MVE.var * (MVE.var list)) list) * liftstate =
    (* variables which are annotated correctly *)
    let vars = atys_of_an aty ans in
    let init_var (v:Il.var) (vs,env:(MVE.var * (MVE.var list)) list * liftstate)
      : (MVE.var * (MVE.var list)) list * liftstate =
      match v.v_ty with
      | Common.Tbase(_) ->
        error (Some v.v_loc)
          "@[cannot lift shared input %a consisting of a single element.@]@."
          Il.V.pp_g v
      | Common.Tarr(bty,rs,re) ->
        begin
          (* need to lift every index as variable *)
          (* need to lift the base variable as well -> true flag *)
          match lift_ilvarr true env v with
          | [], _ ->
            error (Some v.v_loc)
              "@[unexpected error during lifting of shared input variable %a@]@."
              Il.V.pp_g v
          | vb::vs', env ->
            (* return the tuple with the individual shares *)
            [vb,vs']@vs, env
        end
      | Common.Tmem ->
        error (Some v.v_loc)
          "@[Maskverif does not support memory, eliminate %a prior analysis.@]"
          Il.V.pp_g v
    in
    (* initialize/define each random input variable, return the new leakage state*)
    List.fold_right init_var vars ([], env)

  let init_header_sharedoutvars (aty:Ileval.t_ty) (env:liftstate) (ans:(Ileval.t_ty * Il.var) list)
    : (MVE.var list) list * liftstate =
    (* variables which are annotated correctly *)
    let vars = atys_of_an aty ans in
    let init_var (v:Il.var) (vs,env:(MVE.var list) list * liftstate)
      : (MVE.var list) list * liftstate =
      match v.v_ty with
      | Common.Tbase(_) ->
        error (Some v.v_loc)
          "@[cannot lift shared input %a consisting of a single element.@]@."
          Il.V.pp_g v
      | Common.Tarr(bty,rs,re) ->
        begin
          (* need to lift every index as variable *)
          (* need to lift the base variable as well -> true flag *)
          match lift_ilvarr true env v with
          | [], _ ->
            error (Some v.v_loc)
              "@[unexpected error during lifting of shared input variable %a@]@."
              Il.V.pp_g v
          | vb::vs', env ->
            (* return the tuple with the individual shares *)
            vs'::vs, env
        end
      | Common.Tmem ->
        error (Some v.v_loc)
          "@[Maskverif does not support memory, eliminate %a prior analysis.@]"
          Il.V.pp_g v
    in
    (* initialize/define each random input variable, return the new leakage state*)
    List.fold_right init_var vars ([], env)

  let to_instr (env:liftstate) (cmd:Il.cmd) : MVP.cmd * liftstate = [], env

  let to_maskverif (m:Il.macro) (an:Ileval.initial) (st:Ileval.state)
    : MV.Prog.func =
    let f_name = MV.Util.HS.make m.mc_name in
    (* FIXME: not supported in scverif for now *)
    let f_kind = MV.Util.NONE in

    let (lenv:liftstate) = empty_liftstate in

    let f_pin, lenv = init_header_vars Ileval.Public lenv an.input_var in
    let f_rand, lenv = init_header_vars Ileval.URandom lenv an.input_var in
    (* FIXME: shared inputs, not supported in scverif for now *)
    let f_ein = [] in
    let f_in, lenv = init_header_sharedinvars Ileval.Sharing lenv an.input_var in
    let f_out, lenv = init_header_sharedoutvars Ileval.Sharing lenv an.output_var in
    (* body *)
    let f_cmd, lenv = to_instr lenv st.st_eprog in
    let f_other = MVE.Sv.elements lenv.localdefs in
    let (func:MV.Prog.func) = {
      f_name; (* function name *)
      f_pin;  (* public input *)
      f_ein;  (* extra inputs perfectly shared (? shared inputs ?) *)
      f_kind; (* SNI gadget? *)
      f_in;   (* input shares *)
      f_out;  (* output shares *)
      f_other;(* internal variables *)
      f_rand; (* input entropy *)
      f_cmd } in
    (* the rest as in Maskverif.Prog.func *)
    let func = MV.Prog.Process.macro_expand_func mvglob func in
    MV.Prog.add_global lenv.globals func;
    func

end

(* more functionality like checking TBD *)
let to_maskverif_test (m:Il.macro) (an:Ileval.initial) (st:Ileval.state) =
  let func = IlToMv.to_maskverif m an st in
  let pi:MVP.print_info = {var_full = !Glob_option.full; print_info = false} in
  Format.printf "@[lifting returned:@ @[<v>%a]@]@."
    (MV.Prog.pp_func ~full:pi) func
