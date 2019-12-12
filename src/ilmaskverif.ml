(* Copyright 2019 - NXP *)

module MV = Maskverif
module MVP = Maskverif.Prog
module MVE = Maskverif.Expr
module MVU = Maskverif.Util

module IlToMv : sig
  val to_maskverif: Il.macro -> Ileval.initial -> Ileval.state -> MV.Prog.func
  val get_or_lift: Il.macro -> Ileval.initial -> Ileval.state -> MV.Prog.func
  val get_mvprog: Il.macro -> MV.Prog.func

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
      localdefs  : MVE.Sv.t;
      leakdefs   : MVE.Sv.t;
      il2mv      : ilvmapping Il.Mv.t;
      mv2il      : mvvmapping MVE.Mv.t;
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
          "@[Maskverif does not support variable %a with type Int@]@."
          Il.V.pp_dbg v
    in
    match v.v_ty with
    | Common.Tbase(bty) -> lift_bty bty
    | Common.Tarr(bty,_,_) -> lift_bty bty
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
          | true -> MVE.Sv.add mvvar env.localdefs
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

  let o_orb   = mk_op2ttt MVE.w1  "|"
  let o_orw8  = mk_op2ttt MVE.w8  "|w8"
  let o_orw16 = mk_op2ttt MVE.w16 "|w16"
  let o_orw32 = mk_op2ttt MVE.w32 "|w32"
  let o_orw64 = mk_op2ttt MVE.w64 "|w64"

  let o_amulb   = mk_op2ttt MVE.w1  "*"
  let o_amulw8  = mk_op2ttt MVE.w8  "*w8"
  let o_amulw16 = mk_op2ttt MVE.w16 "*w16"
  let o_amulw32 = mk_op2ttt MVE.w32 "*w32"
  let o_amulw64 = mk_op2ttt MVE.w64 "*w64"

  let o_aaddb   = mk_op2tttbij MVE.w1  "+"
  let o_aaddw8  = mk_op2tttbij MVE.w8  "+w8"
  let o_aaddw16 = mk_op2tttbij MVE.w16 "+w16"
  let o_aaddw32 = mk_op2tttbij MVE.w32 "+w32"
  let o_aaddw64 = mk_op2tttbij MVE.w64 "+w64"

  let o_asubb   = mk_op2tttbij MVE.w1  "-"
  let o_asubw8  = mk_op2tttbij MVE.w8  "-w8"
  let o_asubw16 = mk_op2tttbij MVE.w16 "-w16"
  let o_asubw32 = mk_op2tttbij MVE.w32 "-w32"
  let o_asubw64 = mk_op2tttbij MVE.w64 "-w64"

  let rec lift_expr (i:Il.instr) (exty:MVE.ty) (env:liftstate) (expr:Il.expr)
    : MVP.expr * liftstate =
    match expr with
    | Il.Ebool true ->
      MVP.Econst MV.Expr.C._true, env
    | Il.Ebool false ->
      MVP.Econst MV.Expr.C._false, env
    | Il.Eint int ->
      let maxval = MVE.ty_size exty in
      let maxval = Common.B.pow (Common.B.of_int 2) maxval in
      if Common.B.le int maxval then
        let const = MVE.C.make exty (Common.B.to_zint int) in
        MVP.Econst const, env
      else if (function | { Il.i_desc = Il.Ileak _ } -> true | _ -> false) i then
        begin
          (* inside leak statements we type scVerif constant integers as w64 *)
          assert(Common.B.le int (Common.B.pow (Common.B.of_int 2) (MVE.ty_size MVE.w64)));
          let const = MVE.C.make MVE.w64 (Common.B.to_zint int) in
          MVP.Econst const, env
        end
      else
        error (Some (fst i.i_loc))
          "@[overflow detected in %a: expected type %s does not match given integer %a.@]@."
          Il.pp_i_dbg i (MVE.ty2string exty) Common.B.pp_zint int
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
      MVP.Eop(op, mves), env
    in
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
      lift_existing o_orb MVE.W1
    | Il.Oor(Some Common.U8) ->
      lift_existing o_orw8 MVE.W8
    | Il.Oor(Some Common.U16) ->
      lift_existing o_orw16 MVE.W16
    | Il.Oor(Some Common.U32) ->
      lift_existing o_orw32 MVE.W32
    | Il.Oor(Some Common.U64) ->
      lift_existing o_orw64 MVE.W64
    | Il.Oadd(None) ->
      lift_existing o_aaddb MVE.W1
    | Il.Oadd(Some Common.U8) ->
      lift_existing o_aaddw8 MVE.W8
    | Il.Oadd(Some Common.U16) ->
      lift_existing o_aaddw16 MVE.W16
    | Il.Oadd(Some Common.U32) ->
      lift_existing o_aaddw32 MVE.W32
    | Il.Oadd(Some Common.U64) ->
      lift_existing o_aaddw64 MVE.W64
    | Il.Osub(None) ->
      lift_existing o_asubb MVE.W1
    | Il.Osub(Some Common.U8) ->
      lift_existing o_asubw8 MVE.W8
    | Il.Osub(Some Common.U16) ->
      lift_existing o_asubw16 MVE.W16
    | Il.Osub(Some Common.U32) ->
      lift_existing o_asubw32 MVE.W32
    | Il.Osub(Some Common.U64) ->
      lift_existing o_asubw64 MVE.W64
    | Il.Omul(None) ->
      lift_existing o_amulb MVE.W1
    | Il.Omul(Some Common.U8) ->
      lift_existing o_amulw8 MVE.W8
    | Il.Omul(Some Common.U16) ->
      lift_existing o_amulw16 MVE.W16
    | Il.Omul(Some Common.U32) ->
      lift_existing o_amulw32 MVE.W32
    | Il.Omul(Some Common.U64) ->
      lift_existing o_amulw64 MVE.W64
    | Il.Omulh(_) (* pow *)
    | Il.Oopp(_) (* arithmetic negation *)
    | Il.Olsl(_)
    | Il.Olsr(_)
    | Il.Oasr(_)
    | Il.Oeq(_)
    | Il.Olt(_,_)
    | Il.Ole(_,_)
    | Il.Osignextend(_,_)
    | Il.Ozeroextend(_,_) -> err_unsupported ()
    | Il.Oif(_)
    | Il.Ocast_int(_,_)
    | Il.Ocast_w(_) -> err_invalid ()

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
              else if MVE.Sv.mem v env.localdefs then
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
                (MV.Util.pp_list ",@ " (MVE.pp_var)) vs
          end
        else if MVE.Sv.exists (filterbyname vn) env.headerdefs then
          (* this variable has been defined in the header but v.v_uid is fresh *)
          let (mvvar:MVE.var) = MVE.Sv.find_first (filterbyname vn) env.headerdefs in
          let il2mv = Il.Mv.add v (MScalar mvvar) env.il2mv in
          let env:liftstate = {env with il2mv} in
          mvvar, env
        else if MVE.Sv.exists (filterbyname vn) env.localdefs then
          (* this variable has been defined in the header but v.v_uid is fresh *)
          let mvvar:MVE.var = MVE.Sv.find_first (filterbyname vn) env.localdefs in
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
            else if MVE.Sv.exists (filterbyname v.v_name) env.localdefs then
              (* this variable has been defined in the header but v.v_uid is fresh *)
              let mvvar = MVE.Sv.find_first (filterbyname v.v_name) env.localdefs in
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
    let l_name, env =
      match li with
      | Some lname ->
        begin
          (* need to create the variable, add it to the leak definitions *)
          let lv = MVE.V.mk_var lname MVE.w1 in
          let leakdefs = MVE.Sv.add lv env.leakdefs in
          let mv2il = MVE.Mv.add lv (IlLeakname(lname, fst i.i_loc)) env.mv2il in
          lv, { env with leakdefs; mv2il }
        end
      | None ->
        begin
          try MVE.Sv.find_first (filterbyname "unnamedleak") env.leakdefs, env
          with _ ->
            (* create a fresh unnamed variable *)
            let lv = MVE.V.mk_var "unnamedleak" MVE.w1 in
            let leakdefs = MVE.Sv.add lv env.leakdefs in
            let mv2il = MVE.Mv.add lv (IlLeakname("unnamedleak", fst i.i_loc)) env.mv2il in
            lv, { env with leakdefs; mv2il }
        end
    in
    let es', env = List.fold_right fold_lift_expr es ([],env) in
    let l_exprs = MVP.Eop(MVE.o_tuple, es') in
    let instr_d = MVP.Ileak({l_name; l_exprs}) in
    (* TODO improve location *)
    let instr_info = MVP.ToProg.pp_loc_info (lift_illoc (fst i.i_loc)) "" in
    { instr_d; instr_info }::is, env

  let lift_instr (env:liftstate) (is:Il.cmd) : MVP.cmd * liftstate =
    let lift_i (i:Il.instr) (is,env: MVP.cmd * liftstate)
      : MVP.cmd * liftstate =
      match i.i_desc with
      | Il.Iassgn (lvar, expr) ->
        lift_Iassgn i (is,env) lvar expr
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
    : MV.Prog.func =
    if Mf.mem m !globilmacro2func then
      error None
        "@[macro %s already lifted.@]@." m.mc_name;
    let lenv : liftstate = {
      headerdefs = MVE.Sv.empty;
      localdefs = MVE.Sv.empty;
      leakdefs = MVE.Sv.empty;
      il2mv = !globilvar2mv;
      mv2il = !globmvvar2il; } in
    let f_name = MV.Util.HS.make m.mc_name in
    (* FIXME: not supported in scverif for now *)
    let f_kind = MV.Util.NONE in
    let f_pin, lenv = init_header_vars Ileval.Public lenv an.input_var in
    let f_rand, lenv = init_header_vars Ileval.URandom lenv an.input_var in
    (* FIXME: shared inputs, not supported in scverif for now *)
    let f_ein = [] in
    let f_in, lenv = init_header_sharedinvars Ileval.Sharing lenv an.input_var in
    let f_out, lenv = init_header_sharedoutvars Ileval.Sharing lenv an.output_var in
    (* body *)
    let body = List.filter (* remove the labels to keep lift_instr neat and clean *)
        (function | { Il.i_desc = Il.Ilabel _} -> false | _ -> true) st.st_eprog in
    let f_cmd, lenv = lift_instr lenv body in
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
    (* as in Maskverif.Prog.func *)
    let func = MV.Prog.Process.macro_expand_func mvglobalenv func in
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
  Format.printf "@[lifting returned:@ @[<v>%a]@]@."
    (MV.Prog.pp_func ~full:pi) func

let check_mvprog (params:Scv.scvcheckkind) (m:Il.macro) (an:Ileval.initial) (st:Ileval.state) : unit =
  let func = IlToMv.get_or_lift m an st in
  let algorithm =
    (match params with
    | Scv.Noninterference -> MV.Util.(`NI)
    | Scv.Strongnoninterference -> MV.Util.(`SNI))
  in
  let (params, nb_shares, interns, outputs, _) =
    MVP.build_obs_func
      ~trans:false ~glitch:false ~ni:algorithm (IlToMv.lift_illoc m.mc_loc) func in
  let order = (nb_shares - 1) in
  let toolopts:MV.Util.tool_opt = {
    pp_error = true;
    checkbool = true;
  } in
  MV.Checker.check_sni toolopts ~para:true ~fname:(m.mc_name) params nb_shares ~order interns outputs
