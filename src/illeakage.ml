open Utils
open Common
open Location
open Il
open Iltyping

let check_param_eq m ml =
  let param_eq b p1 p2 =
    match p1, p2 with
    | Pvar v1, Pvar v2 -> ty_eq v1.v_ty v2.v_ty
    | Plabel l1, Plabel l2 -> true
    | _, _ -> false in
  try
    List.fold_left2 param_eq true m.mc_params ml.mc_params
  with Invalid_argument _ ->
    false

let add_leakicall genv mname margs =
  let mlname = mname ^ "_leak" in
  match find_macro_opt genv mname, find_macro_opt genv mlname with
  | Some m, Some ml ->
    (if check_param_eq m ml then
      Some {i_desc = Imacro(ml.mc_name, margs); i_loc = (ml.mc_loc, []) }
    else
      error "add_leakicall:" (ml.mc_loc, [])
        "parameters of %s do not match %s" ml.mc_name m.mc_name)
  | _, _ -> None

let rec traverse_code genv instrs =
  match instrs with
  | [] -> []
  | i::is ->
    begin
      match i.i_desc with
      | Imacro(mname, margs) ->
        begin
          match (add_leakicall genv mname margs) with
          | Some il -> il :: i :: traverse_code genv is
          | None -> i :: traverse_code genv is
        end
      | _ -> i :: traverse_code genv is
    end

let addleakage (genv:Iltyping.genv) (mn:Il.macro_name) =
  let m = Iltyping.find_macro genv mn in
  let leakycmds = traverse_code genv m.mc_body in
  let ml = { m with mc_body = leakycmds; mc_uid = Uid.fresh () } in
  let genv = Iltyping.update_macro genv ml in
  genv

module Liveness = Ilcodeelim

let rec substitute_var_expr (v:V.t) (pos:Liveness.lvpos) (esub:Il.expr) (edes:Il.expr) =
  match edes with
  | Il.Evar v' ->
    if V.equal v v' then
      begin
        assert (ty_eq v.v_ty v'.v_ty);
        match v.v_ty with
        | Common.Tbase _ -> esub (* return esub in place of edes *)
        | Common.Tarr (_, _, _) (* the rest is superfluous type-checking *)
        | Common.Tmem ->
          Utils.hierror "Illeakage" None
            "substitute_var: cannot substitute memory and arrays in Evar of Tmem."
      end
    else
      edes (* edes unchanged *)
  | Il.Eget (v', edes') ->
    if V.equal v v' then
      begin
        assert (ty_eq v.v_ty v'.v_ty);
        match v.v_ty with
        | Common.Tarr (_, _, _) ->
          let pos' = Liveness.lvpos_of_expr edes' in
          if Liveness.equal_pos pos pos' then
            esub (* substitution; edes' is static -> no need to descent *)
          else
            (* the variable is the same but the access position is not, thus we descent *)
            Il.Eget(v', substitute_var_expr v pos esub edes')
        | Common.Tbase _ (* the rest is superfluous type-checking *)
        | Common.Tmem ->
          Utils.hierror "Illeakage" None
            "substitute_var: cannot substitute memory and arrays in Eget."
      end
    else
      (* descent into index of get operation *)
      Il.Eget(v', substitute_var_expr v pos esub edes')
  | Il.Eload (ws, v', edes') ->
    begin
      assert(ty_eq v'.v_ty Tmem);
      match v.v_ty with
      | Common.Tmem ->
        (* substitution of memory is not supported, descent into expression *)
        Il.Eload (ws, v', substitute_var_expr v pos esub edes')
      | Common.Tarr (_, _, _)
      | Common.Tbase _ ->
        Utils.hierror "Illeakage" None
          "substitute_var: cannot substitute memory and arrays of Eload."
    end
  | Il.Eop (op, es) ->
    (* descent into expressions *)
    Il.Eop(op, List.map (substitute_var_expr v pos esub) es)
  | Il.Eint _
  | Il.Ebool _ ->
    (* literals should not be replaced *)
    edes

let leaknames_of_scvtarget (is:Il.instr list) (t:Scv.scvtarget) =
  match t with
  | Scv.TIdent [] -> []
  | Scv.TIdent l ->
    List.map unloc l
  | Scv.TWildcard _ ->
    List.fold_right
      (fun i is -> match i.i_desc with
         | Il.Ileak(Some i,_) -> i::is
         | Il.Imacro _ ->
           Utils.hierror "Illeakage.leaknames_of_scvtarget" None
             "encountered a macro call in provided macro, cannot descent into leakage produced by this macro."
         | _ ->  is)
      is []
  | Scv.TRegex r ->
    begin
      let re = unloc r in
      let inames =
        List.fold_right
          (fun i is -> match i.i_desc with
             | Il.Ileak(Some i,_) -> i::is
             | _ ->  is)
          is [] in
      if List.mem re inames then
        [re]
      else
        Utils.hierror "Illeakage.leaknames_of_scvtarget" (Some (loc r))
          "regex %a not yet supported" Scv.pp_scvstring r
    end

(* collect all leakage expressions in a single preceding leak statement *)
let accumulate_leakages (genv:Iltyping.genv) (mn:Il.macro_name) (slt:Scv.scvtarget) (keep:bool) =
  (* decide whether to be selective on leakage and compute list of target leakages to compare to *)
  let m = Iltyping.find_macro genv mn in
  let selective, tls =
    match slt with
    | Scv.TWildcard _ ->
      Glob_option.print_full "accumulation is not selective@."; (false, [])
    | _ ->
      Glob_option.print_full "accumulation is selective@.";
      (true, leaknames_of_scvtarget m.mc_body slt) in
  let lmap = ref Mv.empty in
  let debugadd e = Glob_option.print_full
      "accumulate adding to liveset: %a@." (pp_e ~full:true) e in
  let debugsub e v = Glob_option.print_full
      "accumulate substitute: %a in %a@." V.pp_g v (pp_e ~full:true) e in
  (* collect all leak expression (reverse code traversal) *)
  let collect_leakexpr (i:Il.instr) (es:Il.expr list) =
    begin
      match i.i_desc with
      | Il.Ileak (Some li, expl) when selective ->
        if List.mem li tls then
          begin
            (* mark the variables used in leak expressions as live *)
            lmap := List.fold_left Liveness.liveset_add_expr !lmap expl;
            (* append leak expressions which are not yet part of the list *)
            List.fold_left (fun es enew ->
                if List.mem enew es then es else (debugadd enew; enew::es)) es expl
          end
        else
          es
      | Il.Ileak (None, _) when selective -> es
      | Il.Ileak (_, expl) ->
        begin
          (* mark the variables used in leak expressions as live *)
          lmap := List.fold_left Liveness.liveset_add_expr !lmap expl;
          (* append leak expressions which are not yet part of the list *)
          List.fold_left (fun es enew ->
              if List.mem enew es then es else (debugadd enew; enew::es)) es expl
        end
      | Il.Iassgn(Lvar lv, dexpr) ->
        if Mv.mem lv !lmap then
          begin
            (* update liveset with rhs expr *)
            let lvm = Liveness.liveset_remove_v_pos !lmap lv Liveness.LVbasePos in
            lmap := Liveness.liveset_add_expr lvm dexpr;
            (* substitute lv with expr in es *)
            List.map (fun e -> debugsub e lv; substitute_var_expr lv Liveness.LVbasePos dexpr e) es
          end
        else
          es
      | Il.Iassgn(Lstore(_,lv,iexpr), dexpr)
      | Il.Iassgn(Lset(lv, iexpr), dexpr) ->
        if Mv.mem lv !lmap then
          begin
            (* update liveset with rhs expr *)
            let lmp = Liveness.liveset_remove_v_e !lmap lv iexpr in
            let lmp = Liveness.liveset_add_expr lmp iexpr in
            lmap := Liveness.liveset_add_expr lmp dexpr;
            (* substitute lv with expr in es *)
            List.map (fun e -> debugsub e lv; substitute_var_expr lv (Liveness.lvpos_of_expr iexpr) dexpr e) es
          end
        else
          es
     | Il.Ilabel _ -> es
     | Il.Imacro (_, _) (* FIXME descent? *)
     | Il.Igoto _
     | Il.Iigoto _
     | Il.Iif (_, _, _)
     | Il.Iwhile (_, _, _) ->
       Utils.hierror "accumulate_leakages" None "@[<v>cannot handle@ %a@]"
         (pp_i ~full:false) i
    end in
  let leakexprs =
    List.fold_right collect_leakexpr m.mc_body [] in
  if List.length leakexprs != 0 then
    begin
      (* conditional removal of all other leakage statements *)
      let ml =
        if keep then
          m.mc_body
        else
          begin
            let warn l = Glob_option.eprint_normal
                "Warning: removing @[<v>%a@]@."
                (pp_i ~full:!Glob_option.full) l in
            List.filter
              (fun i ->
                 match i.i_desc with
                 | Il.Ileak (None, _) -> warn i; false
                 | Il.Ileak (Some li,_) ->
                   if selective && not (List.mem li tls) then
                     warn i;
                   false
             | _ -> true )
              m.mc_body
          end in
      (* add the accumulated leakage statement *)
      let il = { i_desc = Il.Ileak(Some "accumulated", leakexprs); i_loc = dummy_full_loc} in
      let ml = { m with mc_body = il::ml; mc_uid = Uid.fresh () } in
      Iltyping.update_macro genv ml
    end
  else
    genv

let filterleakage (eenv:Ileval.eenv) (mn:Il.macro_name) (slt:Scv.scvtarget) (keep:bool) =
  let st = Ileval.find_state eenv mn in
  let eprog = st.st_eprog in
  let ls = leaknames_of_scvtarget eprog slt in
  let stmt_filter instr =
    begin
      match instr.i_desc with
      | Ileak(Some name, _ ) ->
        begin
          if List.mem name ls then
            keep
          else
            not keep
         end
      | Ileak(None, _)
      | Iassgn(_,_)
      | Iigoto _
      | Igoto _
      | Iif _
      | Iwhile _
      | Ilabel _ -> true
      | Imacro _ ->
        Utils.hierror "leakageelim" None "@[<v>cannot handle@ %a@]"
          (pp_i ~full:false) instr
    end in
  let elimprog = List.rev (List.filter stmt_filter (List.rev eprog)) in
  let nst = { st with st_eprog = elimprog } in
  let eenv = Ileval.update_state eenv mn nst in
  eenv
