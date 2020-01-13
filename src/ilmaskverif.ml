(* Copyright 2019 - NXP *)

module MV = Maskverif
module MVP = Maskverif.Prog
module MVE = Maskverif.Expr
module MVU = Maskverif.Util

module IlToMv : sig
  val to_maskverif: Il.macro -> Ileval.initial -> Ileval.state -> MVP.func
  val get_or_lift: Il.macro -> Ileval.initial -> Ileval.state -> MVP.func
  val get_mvprog: Il.macro -> MVP.func

  val lift_illoc: Location.t -> MVU.location

end = struct
  module MTP = Maskverif.Prog.ToProg
  module P = Maskverif.Parsetree

  module Mf = Map.Make(Il.M)

  type ilvmapping =
    | MScalar of MVE.var
    | MArray of MVE.var list
    | MBaseArray of MVE.var * (MVE.var list)

  type mvvmapping =
    | IlVar of Il.var
    | IlArr of Il.var * int
    | IlLeakname of string * Location.t

  (* global lookups *)
  let globilmacro2func : MVP.func Mf.t ref = ref Mf.empty (* lookup from Il.macro -> MVP.func *)
  let globilvar2mv : ilvmapping Il.Mv.t ref = ref Il.Mv.empty (* lookup from Il.var -> MVE.var *)
  let globmvvar2il : mvvmapping MVE.Mv.t ref = ref MVE.Mv.empty (* lookup from MVE.var -> Il.var *)

  let mvglobalenv : MVP.global_env = Hashtbl.create 107 (* maskverif internal env *)

  type liftstate =
    {
      headerdefs : MVE.Sv.t;
      localdefs  : MVE.var Utils.Ms.t;
      leakdefs   : MVE.Sv.t;
      il2mv      : ilvmapping Il.Mv.t; (* translate il variable to mv correspondence *)
      mv2il      : mvvmapping MVE.Mv.t; (* translate back an mv variable to its il origin *)
    }

(*
  let map_ilvar (v:Il.var) : MVE.var =
    Il.Mv.find v !globil2mv

  let map_mvvar (v:MVE.var) : Il.var =
    MVE.Mv.find v !globmv2il
*)

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
    MVU.mkloc (lift_illoc v.v_loc) v.v_name

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

  (* translate il scalar variable to maskverif scalar variable
   * keep the mapping in il2mv and mv2il *)
  let lift_ilvbase (localdef:bool) (env:liftstate) (v:Il.var) : MVE.var * liftstate =
    match v.v_ty with
    | Common.Tbase(bty) ->
      begin
        (* check if variable has already been lifted *)
        if (Il.Mv.mem v env.il2mv) then
          error (Some (v.v_loc))
            "@[variable %a has already been lifted.@]@."
            Il.V.pp_dbg v;

        (* make sure the variable's name has not been defined in the header elsewhere *)
        if (MVE.Sv.exists (fun var -> String.equal var.MVE.v_name v.v_name) env.headerdefs) then
          error (Some (v.v_loc))
            "@[variable %a defined multiple times in the header.@]@."
            Il.V.pp_dbg v;

        (* create the variable with unique id *)
        let mvvar = MVE.V.mk_var v.v_name (lift_ilty v) in
        let headerdefs =
          match not localdef with
          (* add the variable to the set of defined variables *)
          | true -> MVE.Sv.add mvvar env.headerdefs
          | false -> env.headerdefs in
        let localdefs =
          match localdef with
          (* add the variable to the set of defined variables *)
          | true -> Utils.Ms.add mvvar.v_name mvvar env.localdefs
          | false -> env.localdefs in
        (* update the bindings in our mapping *)
        let il2mv = Il.Mv.add v (MScalar mvvar) env.il2mv in
        let mv2il = MVE.Mv.add mvvar (IlVar v) env.mv2il in
        (* return the variable and the new env *)
        mvvar, {env with headerdefs; localdefs; il2mv; mv2il}
      end
    | Common.Tarr(_,_,_) ->
      error (Some v.v_loc)
        "@[use lift_ilvarr instead]"
    | Common.Tmem ->
      error (Some v.v_loc)
        "@[Maskverif does not support memory, eliminate %a prior analysis.@]@."
        Il.V.pp_dbg v

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
              Il.V.pp_dbg v;

          (* make sure the variable's name has not been defined in the header elsewhere *)
          if (MVE.Sv.exists (fun var -> String.equal var.MVE.v_name vname) env.headerdefs) then
            error (Some (v.v_loc))
              "@[variable %a defined multiple times in the header.@]@."
              Il.V.pp_dbg v;

          (* create the variable with unique id *)
          let mvvar = MVE.V.mk_var vname (lift_ilty v) in
          (* add the variable to the set of defined variables *)
          let headerdefs = MVE.Sv.add mvvar env.headerdefs in
          (* update the bindings in our mapping *)
          let mv2il = MVE.Mv.add mvvar (IlArr(v, i)) env.mv2il in
          (* return the variable and the new env *)
          mvvar::vs, {env with headerdefs; mv2il}
        in
        (* lift the individual indices *)
        let mvvars, env = List.fold_right lift_index range ([],env) in
        (* check wether we need to lift the base var as well *)
        if not liftbasevar then
          (* update the binding from il -> mv variable *)
          let il2mv = Il.Mv.add v (MArray mvvars) env.il2mv in
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
                Il.V.pp_dbg v;
            let mvbvar = MVE.V.mk_var v.v_name (lift_ilty v) in
            (* add the variable to the set of defined variables *)
            let headerdefs = MVE.Sv.add mvbvar env.headerdefs in
            (* update the bindings in our mapping *)
            let mv2il = MVE.Mv.add mvbvar (IlVar v) env.mv2il in
            (* update the binding from il -> mv variable *)
            let il2mv = Il.Mv.add v (MBaseArray(mvbvar, mvvars)) env.il2mv in
            (* return the variable and the new env *)
            mvbvar::mvvars, {env with il2mv; headerdefs; mv2il}
          end
      end
    | Common.Tbase(_) ->
      error (Some v.v_loc)
        "@[use lift_ilvbase instead]"
    | Common.Tmem ->
      error (Some v.v_loc)
        "@[Maskverif does not support memory, eliminate %a prior analysis.@]@."
        Il.V.pp_dbg v

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
        let v', env = lift_ilvbase false env v in
        v'::vs, env
      | Common.Tarr(bty,rs,re) ->
        (* need to lift every index as variable *)
        let vs', env = lift_ilvarr false env v in
        vs'@vs, env
      | Common.Tmem ->
        error (Some v.v_loc)
          "@[Maskverif does not support memory, eliminate %a prior analysis.@]"
          Il.V.pp_dbg v
    in
    (* initialize/define each variable, return the new leakage state*)
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
          Il.V.pp_dbg v
      | Common.Tarr(bty,rs,re) ->
        begin
          (* need to lift every index as variable *)
          (* need to lift the base variable as well -> true flag *)
          match lift_ilvarr true env v with
          | [], _ ->
            error (Some v.v_loc)
              "@[unexpected error during lifting of shared input variable %a@]@."
              Il.V.pp_dbg v
          | vb::vs', env ->
            (* return the tuple with the individual shares *)
            [vb,vs']@vs, env
        end
      | Common.Tmem ->
        error (Some v.v_loc)
          "@[Maskverif does not support memory, eliminate %a prior analysis.@]"
          Il.V.pp_dbg v
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
          Il.V.pp_dbg v
      | Common.Tarr(bty,rs,re) ->
        begin
          (* need to lift every index as variable *)
          (* need to lift the base variable as well -> true flag *)
          match lift_ilvarr true env v with
          | [], _ ->
            error (Some v.v_loc)
              "@[unexpected error during lifting of shared input variable %a@]@."
              Il.V.pp_dbg v
          | vb::vs', env ->
            (* return the tuple with the individual shares *)
            vs'::vs, env
        end
      | Common.Tmem ->
        error (Some v.v_loc)
          "@[Maskverif does not support memory, eliminate %a prior analysis.@]"
          Il.V.pp_dbg v
    in
    (* initialize/define each random input variable, return the new leakage state*)
    List.fold_right init_var vars ([], env)

  let check_tbase_exn (l:Location.t) (v:Il.var) : unit =
    match v.v_ty with
    | Common.Tbase(_) -> ()
    | Common.Tarr(_,_,_)
    | Common.Tmem ->
      error (Some l)
        "@[expecting variable %a to have type Tbase.@]@."
        Il.V.pp_dbg v

  let mk_op2ttt t s = MVE.Op.make s (Some ([t;t], t)) MVE.NotBij MVE.Other
  let mk_op2tttbij t s = MVE.Op.make s (Some ([t;t], t)) MVE.Bij MVE.Other

  let o_castsintint = MVE.Op.make "(sint)"    (Some ([MVE.INT], MVE.INT)) MVE.NotBij MVE.Other
  let o_castsintw8  = MVE.Op.make "(sint)w8"  (Some ([MVE.W8],  MVE.INT)) MVE.NotBij MVE.Other
  let o_castsintw16 = MVE.Op.make "(sint)w16" (Some ([MVE.W16], MVE.INT)) MVE.NotBij MVE.Other
  let o_castsintw32 = MVE.Op.make "(sint)w32" (Some ([MVE.W32], MVE.INT)) MVE.NotBij MVE.Other
  let o_castsintw64 = MVE.Op.make "(sint)w64" (Some ([MVE.W64], MVE.INT)) MVE.NotBij MVE.Other

  let o_castuintint = MVE.Op.make "(uint)"    (Some ([MVE.INT], MVE.INT)) MVE.NotBij MVE.Other
  let o_castuintw8  = MVE.Op.make "(uint)w8"  (Some ([MVE.W8],  MVE.INT)) MVE.NotBij MVE.Other
  let o_castuintw16 = MVE.Op.make "(uint)w16" (Some ([MVE.W16], MVE.INT)) MVE.NotBij MVE.Other
  let o_castuintw32 = MVE.Op.make "(uint)w32" (Some ([MVE.W32], MVE.INT)) MVE.NotBij MVE.Other
  let o_castuintw64 = MVE.Op.make "(uint)w64" (Some ([MVE.W64], MVE.INT)) MVE.NotBij MVE.Other

  let o_ltsint = MVE.Op.make "<s"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
  let o_ltsw8  = MVE.Op.make "<sw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
  let o_ltsw16 = MVE.Op.make "<sw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
  let o_ltsw32 = MVE.Op.make "<sw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
  let o_ltsw64 = MVE.Op.make "<sw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

  let o_ltuint = MVE.Op.make "<u"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
  let o_ltuw8  = MVE.Op.make "<uw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
  let o_ltuw16 = MVE.Op.make "<uw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
  let o_ltuw32 = MVE.Op.make "<uw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
  let o_ltuw64 = MVE.Op.make "<uw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

  let o_lesint = MVE.Op.make "<=s"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
  let o_lesw8  = MVE.Op.make "<=sw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
  let o_lesw16 = MVE.Op.make "<=sw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
  let o_lesw32 = MVE.Op.make "<=sw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
  let o_lesw64 = MVE.Op.make "<=sw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

  let o_leuint = MVE.Op.make "<=u"    (Some ([MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
  let o_leuw8  = MVE.Op.make "<=uw8"  (Some ([MVE.W8],  MVE.W1)) MVE.NotBij MVE.Other
  let o_leuw16 = MVE.Op.make "<=uw16" (Some ([MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
  let o_leuw32 = MVE.Op.make "<=uw32" (Some ([MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
  let o_leuw64 = MVE.Op.make "<=uw64" (Some ([MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

  let o_lslw8  = MVE.Op.make "<<w8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
  let o_lslw16 = MVE.Op.make "<<w16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
  let o_lslw32 = MVE.Op.make "<<w32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
  let o_lslw64 = MVE.Op.make "<<w64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

  let o_lsrw8  = MVE.Op.make ">>w8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
  let o_lsrw16 = MVE.Op.make ">>w16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
  let o_lsrw32 = MVE.Op.make ">>w32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
  let o_lsrw64 = MVE.Op.make ">>w64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

  let o_asrw8  = MVE.Op.make ">>sw8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
  let o_asrw16 = MVE.Op.make ">>sw16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
  let o_asrw32 = MVE.Op.make ">>sw32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
  let o_asrw64 = MVE.Op.make ">>sw64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

  let o_castww8  = MVE.Op.make "(w8)"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
  let o_castww16 = MVE.Op.make "(w16)" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
  let o_castww32 = MVE.Op.make "(w32)" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
  let o_castww64 = MVE.Op.make "(w64)" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

  let o_aoppint = MVE.Op.make "-1"    (Some ([MVE.INT], MVE.INT)) MVE.NotBij MVE.Other
  let o_aoppw8  = MVE.Op.make "-1w8"  (Some ([MVE.W8],  MVE.W8)) MVE.NotBij MVE.Other
  let o_aoppw16 = MVE.Op.make "-1w16" (Some ([MVE.W16], MVE.W16)) MVE.NotBij MVE.Other
  let o_aoppw32 = MVE.Op.make "-1w32" (Some ([MVE.W32], MVE.W32)) MVE.NotBij MVE.Other
  let o_aoppw64 = MVE.Op.make "-1w64" (Some ([MVE.W64], MVE.W64)) MVE.NotBij MVE.Other

  let o_eqint = MVE.Op.make "==int" (Some ([MVE.INT;MVE.INT], MVE.W1)) MVE.NotBij MVE.Other
  let o_eqb   = MVE.Op.make "=="    (Some ([MVE.W1;MVE.W1],   MVE.W1)) MVE.NotBij MVE.Other
  let o_eqw8  = MVE.Op.make "==w8"  (Some ([MVE.W8;MVE.W8],   MVE.W1)) MVE.NotBij MVE.Other
  let o_eqw16 = MVE.Op.make "==w16" (Some ([MVE.W16;MVE.W16], MVE.W1)) MVE.NotBij MVE.Other
  let o_eqw32 = MVE.Op.make "==w32" (Some ([MVE.W32;MVE.W32], MVE.W1)) MVE.NotBij MVE.Other
  let o_eqw64 = MVE.Op.make "==w64" (Some ([MVE.W64;MVE.W64], MVE.W1)) MVE.NotBij MVE.Other

  let o_amulint = mk_op2ttt MVE.INT "*"
  let o_amulw8  = mk_op2ttt MVE.w8  "*w8"
  let o_amulw16 = mk_op2ttt MVE.w16 "*w16"
  let o_amulw32 = mk_op2ttt MVE.w32 "*w32"
  let o_amulw64 = mk_op2ttt MVE.w64 "*w64"

  let o_aaddint = mk_op2tttbij MVE.INT  "+"
  let o_aaddw8  = mk_op2tttbij MVE.w8  "+w8"
  let o_aaddw16 = mk_op2tttbij MVE.w16 "+w16"
  let o_aaddw32 = mk_op2tttbij MVE.w32 "+w32"
  let o_aaddw64 = mk_op2tttbij MVE.w64 "+w64"

  let o_asubint = mk_op2tttbij MVE.INT  "-"
  let o_asubw8  = mk_op2tttbij MVE.w8  "-w8"
  let o_asubw16 = mk_op2tttbij MVE.w16 "-w16"
  let o_asubw32 = mk_op2tttbij MVE.w32 "-w32"
  let o_asubw64 = mk_op2tttbij MVE.w64 "-w64"

  let rec lift_expr (i:Il.instr) (exty:MVE.ty) (env:liftstate) (expr:Il.expr)
    : MVP.expr * liftstate =
    match expr with
    | Il.Ebool true ->
      MVP.Econst MVE.C._true, env
    | Il.Ebool false ->
      MVP.Econst MVE.C._false, env
    | Il.Eint bi ->
      MVP.Econst (MVE.C.make MVE.INT (Common.B.to_zint bi)), env
    | Il.Evar v ->
      begin
        (* must be lifted, (no use-before-definition) *)
        match Il.Mv.find v env.il2mv with
        | MScalar mv ->
          MVP.Evar mv, env
        | _ ->
          error (Some v.v_loc)
            "@[unexpected error during lookup of variable which should be defined.@]@."
      end
    | Il.Eget(v, iexpr) ->
      begin
        let index =
          match iexpr with
          | Il.Eint i -> i
          | Il.Ebool true -> Common.B.one
          | Il.Ebool false -> Common.B.zero
          | Il.Evar(_)
          | Il.Eop(_,_)
          | Il.Eget(_,_)
          | Il.Eload(_,_,_) ->
            error (Some (fst i.i_loc))
              "@[Expecting evaluated program, cannot handle variable index %a@]@."
              Il.pp_e_dbg iexpr
        in
        match v.v_ty, Il.Mv.find v env.il2mv with
        | Common.Tarr(bty,rs,re), MArray vs
        | Common.Tarr(bty,rs,re), MBaseArray (_,vs) ->
          begin
            if (not (rs <= index || index <= re)) || not (exty == (lift_ilty v)) then
              error (Some (fst i.i_loc))
                "@[typing errors should never occur here!@]@.";
            let var = List.nth vs (Common.B.to_int (Common.B.sub index rs)) in
            MVP.Evar var, env
          end
        | _, _ ->
          error (Some v.v_loc)
            "@[unexpected error during lookup of lifted indexed-variable.@]@."
      end
    | Il.Eop(op, es) -> lift_op i exty env op es
    | Il.Eload(_,_,_) ->
      error (Some (fst i.i_loc))
        "@[cannot lift memory %a. perform partial evaluation first.@]@."
        Il.pp_e_dbg expr

  and lift_op (i:Il.instr) (exty:MVE.ty) (env:liftstate) (op:Il.op) (es:Il.expr list)
    : MVP.expr * liftstate =
    let err_unsupported () =
      error (Some (fst i.i_loc))
        "@[op %a not supported.@]@."
        Il.pp_e_dbg (Il.Eop(op,es)) in
    let err_invalid () =
      error (Some (fst i.i_loc))
        "@[op %a invalid, perform partial evaluation.@]@."
        Il.pp_e_dbg (Il.Eop(op,es)) in
    let fold_lift_expr ety e (es,env) =
      let e, env = lift_expr i ety env e in
      e::es, env in
    let lift_existing (op:MVE.operator) (ety:MVE.ty) =
      (* TODO      check_ty ety; *)
      let mves, env = List.fold_right (fold_lift_expr ety) es ([],env) in
      match mves with
      | [e] ->
        MVP.Eop1(op, e), env
      | [e1;e2] ->
        MVP.Eop2(op, e1, e2), env
      | _ ->
        MVP.Eop(op, mves), env
    in
    let lift_or (oand:MVE.operator) (onot:MVE.operator) (ety:MVE.ty) =
      let mves, env = List.fold_right (fold_lift_expr ety) es ([],env) in
      match mves with
      | [e1;e2] ->
        (* or a b = not (and (not a) (not b)) *)
        MVP.Eop1(onot, MVP.Eop2(oand, MVP.Eop1(onot, e1), MVP.Eop1(onot,e2))), env
      | [_]
      | _ ->
        error (Some (fst i.i_loc)) "@[unexpected or in %a.@]@."
          Il.pp_e_dbg (Il.Eop(op,es)) in
    match op.od with
    | Il.Oxor(None) ->
      lift_existing MVE.o_addb MVE.W1
    | Il.Oxor(Some Common.U8) ->
      lift_existing MVE.o_addw8 MVE.W8
    | Il.Oxor(Some Common.U16) ->
      lift_existing MVE.o_addw16 MVE.W16
    | Il.Oxor(Some Common.U32) ->
      lift_existing MVE.o_addw32 MVE.W32
    | Il.Oxor(Some Common.U64) ->
      lift_existing MVE.o_addw64 MVE.W64

    | Il.Oand(None) ->
      lift_existing MVE.o_mulb MVE.W1
    | Il.Oand(Some Common.U8) ->
      lift_existing MVE.o_mulw8 MVE.W8
    | Il.Oand(Some Common.U16) ->
      lift_existing MVE.o_mulw16 MVE.W16
    | Il.Oand(Some Common.U32) ->
      lift_existing MVE.o_mulw32 MVE.W32
    | Il.Oand(Some Common.U64) ->
      lift_existing MVE.o_mulw64 MVE.W64

    | Il.Onot(None) ->
      lift_existing MVE.o_negb MVE.W1
    | Il.Onot(Some Common.U8) ->
      lift_existing MVE.o_negw8 MVE.W8
    | Il.Onot(Some Common.U16) ->
      lift_existing MVE.o_negw16 MVE.W16
    | Il.Onot(Some Common.U32) ->
      lift_existing MVE.o_negw32 MVE.W32
    | Il.Onot(Some Common.U64) ->
      lift_existing MVE.o_negw64 MVE.W64

    | Il.Oor(None) ->
      lift_or MVE.o_mulb MVE.o_negb MVE.W1
    | Il.Oor(Some Common.U8) ->
      lift_or MVE.o_mulw8 MVE.o_negw8 MVE.W8
    | Il.Oor(Some Common.U16) ->
      lift_or MVE.o_mulw16 MVE.o_negw16 MVE.W16
    | Il.Oor(Some Common.U32) ->
      lift_or MVE.o_mulw32 MVE.o_negw32 MVE.W32
    | Il.Oor(Some Common.U64) ->
      lift_or MVE.o_mulw64 MVE.o_negw64 MVE.W64

    | Il.Oadd(None) ->
      lift_existing o_aaddint MVE.W1
    | Il.Oadd(Some Common.U8) ->
      lift_existing o_aaddw8 MVE.W8
    | Il.Oadd(Some Common.U16) ->
      lift_existing o_aaddw16 MVE.W16
    | Il.Oadd(Some Common.U32) ->
      lift_existing o_aaddw32 MVE.W32
    | Il.Oadd(Some Common.U64) ->
      lift_existing o_aaddw64 MVE.W64

    | Il.Osub(None) ->
      lift_existing o_asubint MVE.W1
    | Il.Osub(Some Common.U8) ->
      lift_existing o_asubw8 MVE.W8
    | Il.Osub(Some Common.U16) ->
      lift_existing o_asubw16 MVE.W16
    | Il.Osub(Some Common.U32) ->
      lift_existing o_asubw32 MVE.W32
    | Il.Osub(Some Common.U64) ->
      lift_existing o_asubw64 MVE.W64

    | Il.Omul(None) ->
      lift_existing o_amulint MVE.W1
    | Il.Omul(Some Common.U8) ->
      lift_existing o_amulw8 MVE.W8
    | Il.Omul(Some Common.U16) ->
      lift_existing o_amulw16 MVE.W16
    | Il.Omul(Some Common.U32) ->
      lift_existing o_amulw32 MVE.W32
    | Il.Omul(Some Common.U64) ->
      lift_existing o_amulw64 MVE.W64

    | Il.Oopp(None) ->
      lift_existing o_aoppint MVE.W1
    | Il.Oopp(Some Common.U8) ->
      lift_existing o_aoppw8 MVE.W8
    | Il.Oopp(Some Common.U16) ->
      lift_existing o_aoppw16 MVE.W16
    | Il.Oopp(Some Common.U32) ->
      lift_existing o_aoppw32 MVE.W32
    | Il.Oopp(Some Common.U64) ->
      lift_existing o_aoppw64 MVE.W64

    | Il.Ocast_int(Signed, None) ->
      lift_existing o_castsintint MVE.W1
    | Il.Ocast_int(Signed, Some Common.U8) ->
      lift_existing o_castsintw8 MVE.W8
    | Il.Ocast_int(Signed, Some Common.U16) ->
      lift_existing o_castsintw16 MVE.W16
    | Il.Ocast_int(Signed, Some Common.U32) ->
      lift_existing o_castsintw32 MVE.W32
    | Il.Ocast_int(Signed, Some Common.U64) ->
      lift_existing o_castsintw64 MVE.W64

    | Il.Ocast_int(Unsigned, None) ->
      lift_existing o_castuintint MVE.W1
    | Il.Ocast_int(Unsigned, Some Common.U8) ->
      lift_existing o_castuintw8 MVE.W8
    | Il.Ocast_int(Unsigned, Some Common.U16) ->
      lift_existing o_castuintw16 MVE.W16
    | Il.Ocast_int(Unsigned, Some Common.U32) ->
      lift_existing o_castuintw32 MVE.W32
    | Il.Ocast_int(Unsigned, Some Common.U64) ->
      lift_existing o_castuintw64 MVE.W64

    | Il.Olt(Signed, None) ->
      lift_existing o_ltsint MVE.W1
    | Il.Olt(Signed, Some Common.U8) ->
      lift_existing o_ltsw8 MVE.W8
    | Il.Olt(Signed, Some Common.U16) ->
      lift_existing o_ltsw16 MVE.W16
    | Il.Olt(Signed, Some Common.U32) ->
      lift_existing o_ltsw32 MVE.W32
    | Il.Olt(Signed, Some Common.U64) ->
      lift_existing o_ltsw64 MVE.W64

    | Il.Olt(Unsigned, None) ->
      lift_existing o_ltuint MVE.W1
    | Il.Olt(Unsigned, Some Common.U8) ->
      lift_existing o_ltuw8 MVE.W8
    | Il.Olt(Unsigned, Some Common.U16) ->
      lift_existing o_ltuw16 MVE.W16
    | Il.Olt(Unsigned, Some Common.U32) ->
      lift_existing o_ltuw32 MVE.W32
    | Il.Olt(Unsigned, Some Common.U64) ->
      lift_existing o_ltuw64 MVE.W64

    | Il.Ole(Signed, None) ->
      lift_existing o_lesint MVE.W1
    | Il.Ole(Signed, Some Common.U8) ->
      lift_existing o_lesw8 MVE.W8
    | Il.Ole(Signed, Some Common.U16) ->
      lift_existing o_lesw16 MVE.W16
    | Il.Ole(Signed, Some Common.U32) ->
      lift_existing o_lesw32 MVE.W32
    | Il.Ole(Signed, Some Common.U64) ->
      lift_existing o_lesw64 MVE.W64

    | Il.Ole(Unsigned, None) ->
      lift_existing o_leuint MVE.W1
    | Il.Ole(Unsigned, Some Common.U8) ->
      lift_existing o_leuw8 MVE.W8
    | Il.Ole(Unsigned, Some Common.U16) ->
      lift_existing o_leuw16 MVE.W16
    | Il.Ole(Unsigned, Some Common.U32) ->
      lift_existing o_leuw32 MVE.W32
    | Il.Ole(Unsigned, Some Common.U64) ->
      lift_existing o_leuw64 MVE.W64

    | Il.Olsl(Common.U8) ->
      lift_existing o_lslw8 MVE.W8
    | Il.Olsl(Common.U16)->
      lift_existing o_lslw16 MVE.W16
    | Il.Olsl(Common.U32)->
      lift_existing o_lslw32 MVE.W32
    | Il.Olsl(Common.U64)->
      lift_existing o_lslw64 MVE.W64

    | Il.Olsr(Common.U8) ->
      lift_existing o_lsrw8 MVE.W8
    | Il.Olsr(Common.U16)->
      lift_existing o_lsrw16 MVE.W16
    | Il.Olsr(Common.U32)->
      lift_existing o_lsrw32 MVE.W32
    | Il.Olsr(Common.U64)->
      lift_existing o_lsrw64 MVE.W64

    | Il.Oasr(Common.U8) ->
      lift_existing o_asrw8 MVE.W8
    | Il.Oasr(Common.U16)->
      lift_existing o_asrw16 MVE.W16
    | Il.Oasr(Common.U32)->
      lift_existing o_asrw32 MVE.W32
    | Il.Oasr(Common.U64)->
      lift_existing o_asrw64 MVE.W64

    | Il.Ocast_w(Common.U8) ->
      lift_existing o_castww8 MVE.W8
    | Il.Ocast_w(Common.U16)->
      lift_existing o_castww16 MVE.W16
    | Il.Ocast_w(Common.U32)->
      lift_existing o_castww32 MVE.W32
    | Il.Ocast_w(Common.U64)->
      lift_existing o_castww64 MVE.W64

    | Il.Oeq (Common.Bool) ->
      lift_existing o_eqb MVE.W8
    | Il.Oeq (Common.Int) ->
      lift_existing o_eqint MVE.W8
    | Il.Oeq (W Common.U8)->
      lift_existing o_eqw8 MVE.W8
    | Il.Oeq (W Common.U16)->
      lift_existing o_eqw16 MVE.W16
    | Il.Oeq (W Common.U32)->
      lift_existing o_eqw32 MVE.W32
    | Il.Oeq (W Common.U64)->
      lift_existing o_eqw64 MVE.W64

    | Il.Osignextend(ws1, ws2)
    | Il.Ozeroextend(ws1, ws2) -> err_unsupported ()
    | Il.Oif(ty) -> err_invalid ()
    (* pow *)
    | Il.Omulh(ws) -> err_unsupported ()

  let lift_Iassgn (i:Il.instr) (is,env: MVP.cmd * liftstate) (lvar:Il.lval) (rhs:Il.expr)
    : MVP.cmd * liftstate =
    let (i_var, env : MVE.var * liftstate) =
      let filterbyname (nm:string) (mvv:MVE.var) : bool = String.equal nm mvv.MVE.v_name in
      match lvar with
      (* assignment to variable *)
      | Il.Lvar(v) ->
        let vn = v.v_name in
        (* check if it is already lifted *)
        if Il.Mv.mem v env.il2mv then
          begin
            match Il.Mv.find v env.il2mv with
            | MScalar v ->
              if MVE.Sv.mem v env.headerdefs then
                v, env
              else if Utils.Ms.mem v.v_name env.localdefs then
                v, env
              else
                error (Some (fst i.i_loc))
                  "@[unexpected error during lookup, \
                   lifted variable %a was not defined in header@]@."
                  MVE.pp_var v
            | MArray(vs)
            | MBaseArray(_,vs) ->
              error (Some v.v_loc)
                "@[unexpected error during lookup, \
                 lifted variable %a is a set of variables %a.@]@."
                Il.V.pp_dbg v
                (MVU.pp_list ",@ " (MVE.pp_var)) vs
          end
        else if MVE.Sv.exists (filterbyname vn) env.headerdefs then
          (* this variable has been defined in the header but v.v_uid is fresh *)
          let (mvvar:MVE.var) =
            try MVE.Sv.find_first (filterbyname vn) env.headerdefs
            with Not_found ->
              MVE.Sv.iter (fun v -> Format.printf "  %a.%d@." MVE.pp_var v v.v_id) env.headerdefs;
              error (Some v.v_loc)
                "@[unexpected error during lookup, \
                 non-lifted variable %a exists but cannot be found %s.@]@."
                Il.V.pp_dbg v vn;
          in
          let il2mv = Il.Mv.add v (MScalar mvvar) env.il2mv in
          let env:liftstate = {env with il2mv} in
          mvvar, env
        else if Utils.Ms.mem vn env.localdefs then
          (* this variable has been defined in the header but v.v_uid is fresh *)
          let mvvar:MVE.var = Utils.Ms.find vn env.localdefs in
          let il2mv = Il.Mv.add v (MScalar mvvar) env.il2mv in
          mvvar, {env with il2mv}
        else
          begin
            (* need to lift the variable, add it to the local definitions *)
            lift_ilvbase true env v
          end
      (* assignment to array with given index *)
      | Il.Lset(v,iexpr) ->
        begin
          let index =
            match iexpr with
            | Il.Eint i -> i
            | Il.Ebool true -> Common.B.one
            | Il.Ebool false -> Common.B.zero
            | Il.Evar(_)
            | Il.Eop(_,_)
            | Il.Eget(_,_)
            | Il.Eload(_,_,_) ->
              error (Some (fst i.i_loc))
                "@[Expecting evaluated program, cannot handle variable index %a@]@."
                Il.pp_e_dbg iexpr
          in
          let mvname = v.v_name ^ Common.B.to_string index in
          match v.v_ty with
          | Common.Tarr(_,rs,re) ->
            (* check bounds *)
            if not (rs <= index || index <= re) then
              error (Some (fst i.i_loc))
                "@[index out of range, typing errors should never occur here!@]@.";
            (* check if it is already lifted *)
            if Il.Mv.mem v env.il2mv then
              begin
                match Il.Mv.find v env.il2mv with
                | MScalar v' ->
                  error (Some v.v_loc)
                    "@[unexpected error during lookup of lifted indexed-variable.@]@."
                | MArray vs
                | MBaseArray(_,vs) ->
                  List.nth vs (Common.B.to_int (Common.B.sub index rs)), env
              end
            else if MVE.Sv.exists (filterbyname mvname) env.headerdefs then
              (* this variable has been defined in the header but uid of il var is fresh *)
              let mvvar = MVE.Sv.find_first (filterbyname mvname) env.headerdefs in
              (* copy the entry *)
              let v' =
                match MVE.Mv.find mvvar env.mv2il with
                | IlVar(v) -> v
                | IlArr(v,_) -> v
                | IlLeakname(_) ->
                  error (Some (fst i.i_loc))
                    "@[unexpected typing error. leakage variable reused.@]@."
              in
              let mvvars_full = Il.Mv.find v' env.il2mv in
              (* add the alias *)
              let il2mv = Il.Mv.add v mvvars_full env.il2mv in
              mvvar, {env with il2mv}
            else if Utils.Ms.mem v.v_name env.localdefs then
              (* this variable has been defined in the header but v.v_uid is fresh *)
              let mvvar = Utils.Ms.find v.v_name env.localdefs in
              (* copy the entry *)
              let v' =
                match MVE.Mv.find mvvar env.mv2il with
                | IlVar(v) -> v
                | IlArr(v,_) -> v
                | IlLeakname(_) ->
                  error (Some (fst i.i_loc))
                    "@[unexpected typing error. leakage variable reused.@]@."
              in
              let mvvars_full = Il.Mv.find v' env.il2mv in
              (* add the alias *)
              let il2mv = Il.Mv.add v mvvars_full env.il2mv in
              mvvar, {env with il2mv}
            else
              begin
                (* need to lift the variable, add it to the local definitions *)
                let mvvars, env = lift_ilvarr true env v in
                List.nth mvvars (Common.B.to_int (Common.B.sub index rs)), env
              end
          | Common.Tbase(_) ->
            assert false (* typing error *)
          | Common.Tmem ->
            error (Some (fst i.i_loc))
              "@[Expecting evaluated program, cannot handle %a@]@."
              Il.pp_i_dbg i
        end
      (* assignment to memory *)
      | Il.Lstore(_,_,_) ->
        error (Some (fst i.i_loc))
          "@[Expecting evaluated program, cannot handle %a@]@."
          Il.pp_i_dbg i
    in
    let i_kind = MV.Parsetree.IK_noleak in
    let i_expr, env = lift_expr i (i_var.v_ty) env rhs in
    let instr_d = MVP.Iassgn({i_var; i_kind; i_expr}) in
    (* TODO improve location *)
    let instr_info = MVP.ToProg.pp_loc_info (lift_illoc (fst i.i_loc)) "" in
    { instr_d; instr_info }::is, env

  let lift_Ileak (i:Il.instr) (is,env: MVP.cmd * liftstate) (li:Il.leak_info) (es:Il.expr list)
    : MVP.cmd * liftstate =
    let fold_lift_expr e (es,env) =
      let e, env = lift_expr i MVE.w1 env e in
      e::es, env in
    let filterbyname (nm:string) (mvv:MVE.var) : bool = String.equal nm mvv.MVE.v_name in
    let l_name, lis,  env =
      match li with
      | Some lname ->
        begin
          (* need to create the variable, add it to the leak definitions *)
          let lv = MVE.V.mk_var lname MVE.w1 in
          let leakdefs = MVE.Sv.add lv env.leakdefs in
          let mv2il = MVE.Mv.add lv (IlLeakname(lname, fst i.i_loc)) env.mv2il in
          let lis = Format.sprintf "@[leak %s at %s@]"
              lname
              (List.fold_right
                 (fun l s -> String.concat "\n" [Location.tostring l; s]) (snd i.i_loc) "") in
          lv, lis, { env with leakdefs; mv2il }
        end
      | None ->
        begin
          let lis = Format.sprintf "@[leak _ at %s@]"
              (List.fold_right
                 (fun l s -> String.concat "\n" [Location.tostring l; s]) (snd i.i_loc) "") in
          try MVE.Sv.find_first (filterbyname "unnamedleak") env.leakdefs, lis, env
          with _ ->
            (* create a fresh unnamed variable *)
            let lv = MVE.V.mk_var "unnamedleak" MVE.w1 in
            let leakdefs = MVE.Sv.add lv env.leakdefs in
            let mv2il = MVE.Mv.add lv (IlLeakname("unnamedleak", fst i.i_loc)) env.mv2il in
            lv, lis, { env with leakdefs; mv2il }
        end
    in
    let es', env = List.fold_right fold_lift_expr es ([],env) in
    let l_exprs = MVP.Eop(MVE.o_tuple, es') in
    let instr_d = MVP.Ileak({l_name; l_exprs}) in
        let instr_info = MVP.ToProg.pp_loc_info (lift_illoc (fst i.i_loc)) lis in
    { instr_d; instr_info }::is, env

  let lift_instr (env:liftstate) (is:Il.cmd) : MVP.cmd * liftstate =
    let lift_i (i:Il.instr) (is,env: MVP.cmd * liftstate)
      : MVP.cmd * liftstate =
      match i.i_desc with
      | Il.Iassgn (lvar, expr) ->
        lift_Iassgn i (is, env) lvar expr
      | Il.Ileak (li, es) ->
        lift_Ileak i (is,env) li es
      | Il.Imacro (_, _) ->
        error (Some (fst i.i_loc))
          "@[macro calls not yet supported: cannot handle %a@]@."
          Il.pp_i_dbg i
      | Il.Ilabel _ ->
        error (Some (fst i.i_loc))
          "@[sorry, you are hitting a regression at label %a@]@."
          Il.pp_i_dbg i
      | Il.Igoto _
      | Il.Iigoto _
      | Il.Iif (_, _, _)
      | Il.Iwhile (_, _, _) ->
        error (Some (fst i.i_loc))
          "@[Expecting evaluated program, cannot handle %a@]@."
          Il.pp_i_dbg i
    in
    let is, env = List.fold_right lift_i (List.rev is) ([],env) in
    List.rev is, env

  let to_maskverif (m:Il.macro) (an:Ileval.initial) (st:Ileval.state)
    : MVP.func =
    if Mf.mem m !globilmacro2func then
      error None
        "@[macro %s already lifted.@]@." m.mc_name;
    let lenv : liftstate = {
      headerdefs = MVE.Sv.empty;
      localdefs = Utils.Ms.empty;
      leakdefs = MVE.Sv.empty;
      il2mv = !globilvar2mv;
      mv2il = !globmvvar2il; } in
    let f_name = MVU.HS.make m.mc_name in
    let f_kind = MVU.NONE in
    let f_pin, lenv = init_header_vars Ileval.Public lenv an.input_var in
    let f_rand, lenv = init_header_vars Ileval.URandom lenv an.input_var in
    let f_pout, lenv = init_header_vars Ileval.Public lenv an.output_var in
    let f_ein = [] in
    let f_in, lenv = init_header_sharedinvars Ileval.Sharing lenv an.input_var in
    let f_out, lenv = init_header_sharedoutvars Ileval.Sharing lenv an.output_var in
    (* body *)
    let body = List.filter (* remove the labels to keep lift_instr neat and clean *)
        (function | { Il.i_desc = Il.Ilabel _} -> false | _ -> true) st.st_eprog in
    let f_cmd, lenv = lift_instr lenv body in
    let f_other = Utils.Ms.fold (fun vn v ls -> v::ls) lenv.localdefs [] in
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
    globilvar2mv := lenv.il2mv;
    globmvvar2il := lenv.mv2il;
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
    | Scv.Noninterference -> MVU.(`NI)
    | Scv.Strongnoninterference -> MVU.(`SNI))
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
  | Scv.Noninterference ->
    MV.Checker.check_ni
      toolopts ~para:true ~fname:(m.mc_name) mvparams nb_shares ~order interns
  | Scv.Strongnoninterference ->
    MV.Checker.check_sni
      toolopts ~para:true ~fname:(m.mc_name) mvparams nb_shares ~order
      interns ~outpub:pubout outputs in
  if success then
    Format.printf "successful check@."
  else
    Format.printf "failed check@."
