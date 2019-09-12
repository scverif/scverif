open Utils
open Common
open Location
open Il
open Iltyping

type lvpos =
  | LVbasePos
  | LVoPos of B.zint list
  | LVunknownPos (* TODO make an optional expression list to enable equality for unknown lvpos *)
[@@deriving show]

let merge_pos lv1 lv2 =
  match lv1, lv2 with
  | LVunknownPos, _
  | _           , LVunknownPos ->
    LVunknownPos
  | LVoPos l1   , LVoPos l2 ->
    LVoPos (List.sort_uniq B.compare (List.append l1 l2))
  | LVoPos _    , LVbasePos
  | LVbasePos   , LVoPos _ ->
    Utils.hierror "deadcodeelim" None
      "merge_pos: cannot merge base and index.@"
  | LVbasePos     , LVbasePos -> LVbasePos

let equal_pos p1 p2 =
  match p1, p2 with
  | LVbasePos, LVbasePos -> true
  | LVoPos l1, LVoPos l2 ->
    if List.length l1 == List.length l2 then
      begin
        (* in both lists, there is no element which is not part of the other list *)
        not (List.exists (fun e -> not (List.mem e l2)) l1) &&
        not (List.exists (fun e -> not (List.mem e l1)) l2)
        (* constant-time version ;-)
           let b = List.fold_right (fun e b -> b && List.mem e l2) l1  true in
           List.fold_right (fun e b -> b && List.mem e l1) l2 b                   *)
      end
    else
      false
  | LVunknownPos, LVunknownPos -> false (* by definition, yet might be equal *)
  | _ , _ -> false

let liveset_add_v_pos lmap var pos =
  if Mv.mem var lmap then
    let vo = Mv.find var lmap in
    Mv.update var var (merge_pos pos vo) lmap
  else
    Mv.add var pos lmap

let lvpos_of_var (v:V.t) =
  match v.v_ty with
  | Common.Tbase _ ->
    LVbasePos
  | Common.Tarr (_, i1, i2) ->
    let rec range s e =
      if s > e then []
      else B.of_int s :: range (s + 1) e in
    LVoPos (range (B.to_int i1) (B.to_int i2))
  | Common.Tmem ->
    LVunknownPos

let rec lvpos_of_expr e =
  match e with
  | Il.Eint i ->
    (* constant position access *)
    LVoPos [i]
  | Il.Evar _ ->
    (* access based on unknown variable *)
    LVunknownPos
  | Il.Eget (_, _)
  | Il.Eload (_, _, _) ->
    (* access based on unknown indexed-variable*)
    LVunknownPos
  | Il.Eop (_, e2) ->
    (* access based on operation, descent in expressions and merge pos *)
    let rec inner es =
      match es with
      (* single or last expression *)
      | e::[] -> lvpos_of_expr e
      (* two or more expressions *)
      | e1::es ->
        let epos1 = lvpos_of_expr e1 in
        let epos2 = inner es in
        merge_pos epos1 epos2
      (* there should not be operations on empty expr-list after typechecking *)
      | [] -> assert false in
    inner e2
  | Il.Ebool _ ->
    Utils.hierror "liveness" None
      "indexed variable access based on a bool, undefined behavior. %a@"
      (pp_e ~full:!Glob_option.full) e

let liveset_add_v lmap var =
  liveset_add_v_pos lmap var (lvpos_of_var var)

let rec liveset_add_expr lmap e =
  match e with
  | Il.Eint _
  | Il.Ebool _ ->
    lmap (* no update for constants *)
  | Il.Evar v ->
    (* add variable v to liveset *)
    liveset_add_v lmap v
  | Il.Eop (_, e) ->
    (* descent into expressions of operation *)
    List.fold_left liveset_add_expr lmap e
  | Il.Eget (v, e)
  | Il.Eload (_, v, e) ->
    (* compute positions of indexed-variable access *)
    let lmap,lpos = lvpos_of_e lmap e in
    liveset_add_v_pos lmap v lpos

and lvpos_of_e lmap e =
  match e with
  | Il.Eint i ->
    (* constant position access *)
    lmap, LVoPos [i]
  | Il.Evar v ->
    (* access based on variable=> unknown position
     * add the variable to liveset *)
    liveset_add_v lmap v, LVunknownPos
  | Il.Eget (v, e)
  | Il.Eload (_, v, e) ->
    (* access based on indexed-variable => unknown position
     * add the variable and descent into the expression *)
    let lmap = liveset_add_v lmap v in
    liveset_add_expr lmap e, LVunknownPos
  | Il.Eop (_, e2) ->
    (* access based on operation, descent in expressions and merge pos *)
    let rec inner lmap es =
      match es with
      (* single or last expression *)
      | e::[] -> lvpos_of_e lmap e
      (* two or more expressions *)
      | e1::es ->
        let lmap1, epos1 = lvpos_of_e lmap e1 in
        let lmap2, epos2 = inner lmap1 es in
        (lmap2, (merge_pos epos1 epos2))
      (* no ops on empty expr-list after typechecking *)
      | [] -> assert false in
    inner lmap e2
  | Il.Ebool _ ->
    Utils.hierror "deadcodeelim" None
      "indexed variable access based on a bool, no idea what to do. %a@"
      (pp_e ~full:!Glob_option.full) e

let liveset_remove_v_is lmap v is =
  try
    match Mv.find v lmap with
    | LVunknownPos -> lmap
    | LVbasePos ->
      Utils.hierror "deadcodeelim" None
        "cannot remove a position of variable %a as it is live on base.@"
        V.pp_g v
    | LVoPos l ->
      let l' = List.filter (fun e -> not (List.mem e is)) l in
      if List.length l' == 0 then
        Mv.remove v lmap
      else
        Mv.update v v (LVoPos l') lmap
  with Not_found ->
    lmap

let liveset_remove_v_e lmap v e =
  let lmap, pos = lvpos_of_e lmap e in
  match pos with
  | LVbasePos ->
    assert false
  | LVoPos l ->
    liveset_remove_v_is lmap v l
  | LVunknownPos ->
    lmap

let liveset_remove_v_pos lmap v pos =
  match pos with
  | LVbasePos ->
    (try
       match Mv.find v lmap with
       | LVbasePos -> Mv.remove v lmap
       | LVoPos _ ->
         Utils.hierror "deadcodeelim" None
           "cannot remove base of variable %a as it is live on a position.@"
           V.pp_g v
       | LVunknownPos -> lmap
     with Not_found -> lmap)
  | LVunknownPos ->
    Utils.hierror "deadcodeelim" None
      "cannot remove unknown position of variable %a.@"
      V.pp_g v
  | LVoPos l ->
    liveset_remove_v_is lmap v l

let is_live_v_i lmap var i =
  let livefilter key value =
    if V.equal key var then
      match value with
      | LVoPos l -> List.mem i l
      | _ -> false
    else false in
  Mv.exists livefilter lmap

let infer_inputs (ainv:(Ileval.t_ty * V.t) list) lmap =
  let aninpt = List.fold_left (fun s (t,v) -> Mv.add v t s) Mv.empty ainv in
  let conv_live_to_annot lv pos cnvrtmp =
    if Mv.mem lv cnvrtmp then
      cnvrtmp
    else
      (* TODO better inference of security type *)
      Mv.add lv Ileval.Public cnvrtmp in
  let inputmap = Mv.fold conv_live_to_annot lmap aninpt in
  List.fold_right (fun (x,y) l -> (y,x)::l) (Mv.bindings inputmap) []

let liveset_of_annot aoutv =
  let lmap = Mv.empty in
  List.fold_left (fun m (_,v) -> liveset_add_v m v) lmap aoutv

let deadcodeelim eenv m =
  let st = Ileval.find_state eenv m.mc_name in
  let eprog = st.st_eprog in
  let annot = Ileval.find_initial eenv m.mc_name in
  let lmap = ref (liveset_of_annot annot.output_var) in
  let is_livestmt instr =
    begin
      match instr.i_desc with
      | Iassgn(Lvar lv, dexpr) ->
        if Mv.mem lv !lmap then
          begin
            let lvm = liveset_remove_v_pos !lmap lv LVbasePos in
            lmap := liveset_add_expr lvm dexpr;
            true
          end
        else
          false
      | Iassgn(Lstore(_,lv,iexpr), dexpr)
      | Iassgn(Lset(lv, iexpr), dexpr) ->
        if Mv.mem lv !lmap then
          begin
            let lmp = liveset_remove_v_e !lmap lv iexpr in
            let lmp = liveset_add_expr lmp iexpr in
            lmap := liveset_add_expr lmp dexpr;
            true
          end
        else
          false
      | Ileak(_, dexpr) ->
        lmap := List.fold_left liveset_add_expr !lmap dexpr;
        true
      | Iigoto _ (* FIXME *)
      | Igoto _
      | Ilabel _ -> false
      | Iif _
      | Iwhile _
      | Imacro _ ->
        Utils.hierror "deadcodeelim" None "@[<v>cannot handle@ %a@]"
          (pp_i ~full:false) instr
    end in
  let elimprog = List.rev (List.filter is_livestmt (List.rev eprog)) in
  let nan = {annot with input_var = infer_inputs annot.input_var !lmap } in
  let eenv = Ileval.update_initial eenv m nan in
  let nst = { st with st_eprog = elimprog } in
  let eenv = Ileval.update_state eenv m nst in
  eenv

let leakageelim eenv m ls (keep:bool) =
  let st = Ileval.find_state eenv m.mc_name in
  let eprog = st.st_eprog in
  let ls = List.map unloc ls in
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
  let eenv = Ileval.update_state eenv m nst in
  eenv
