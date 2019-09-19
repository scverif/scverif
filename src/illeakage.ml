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

let add_leakage genv m =
  let leakycmds = traverse_code genv m.mc_body in
  let ml = {m with mc_body = leakycmds } in
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
          Utils.hierror "illeakage" None
            "substitute_var: cannot substitute memory and arrays here.@"
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
          Utils.hierror "illeakage" None
            "substitute_var: cannot substitute memory and arrays here.@"
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
        Utils.hierror "illeakage" None
          "substitute_var: cannot substitute memory and arrays here.@"
    end
  | Il.Eop (op, es) ->
    (* descent into expressions *)
    Il.Eop(op, List.map (substitute_var_expr v pos esub) es)
  | Il.Eint _
  | Il.Ebool _ ->
    (* literals should not be replaced *)
    edes

(* collect all leakage expressions in a single preceding leak statement *)
let accumulate_leakages genv m =
  let lmap = ref Mv.empty in
  (* collect all leak expression (reverse code traversal) *)
  let collect_leakexpr (i:Il.instr) (es:Il.expr list) =
    (match i.i_desc with
     | Il.Ileak (_, expl) ->
       begin
         (* mark the variables used in leak expressions as live *)
         lmap := List.fold_left Liveness.liveset_add_expr !lmap expl;
         (* append leak expressions which are not yet part of the list *)
         List.fold_left (fun es enew -> if List.mem enew es then es else enew::es) es expl
       end
      | Il.Iassgn(Lvar lv, dexpr) ->
        if Mv.mem lv !lmap then
          begin
            (* update liveset with rhs expr *)
            let lvm = Liveness.liveset_remove_v_pos !lmap lv Liveness.LVbasePos in
            lmap := Liveness.liveset_add_expr lvm dexpr;
            (* substitute lv with expr in es *)
            List.map (substitute_var_expr lv Liveness.LVbasePos dexpr) es
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
            List.map (substitute_var_expr lv (Liveness.lvpos_of_expr iexpr) dexpr) es
          end
        else
          es
     | Il.Imacro (_, _) (* FIXME do not descent? *)
     | Il.Ilabel _ -> es
     | Il.Igoto _
     | Il.Iigoto _
     | Il.Iif (_, _, _)
     | Il.Iwhile (_, _, _) ->
       Utils.hierror "accumulate_leakages" None "@[<v>cannot handle@ %a@]"
         (pp_i ~full:false) i) in
  let leakexprs =
    List.fold_right collect_leakexpr m.mc_body [] in
  if List.length leakexprs != 0 then
    begin
      (* conditional removal of all other leakage statements *)
      let keep = false in
      let ml = List.filter
          (fun i ->
             match i.i_desc with
             | Il.Ileak (_) -> keep
             | _ -> true )
          m.mc_body in
    (* add the accumulated leakage statement *)
      let il = { i_desc = Il.Ileak(Some "accumulated", leakexprs); i_loc = dummy_full_loc} in
      let ml = {m with mc_body = il::ml} in
      Iltyping.update_macro genv ml
    end
  else
    genv
