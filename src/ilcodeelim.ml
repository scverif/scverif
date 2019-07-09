open Utils
open Common
open Location
open Il
open Iltyping

let rec liveset_of_expr lset expr =
  match expr with
  | Evar v -> Sv.add v lset
  | Eload(_,v,e)
  | Eget(v,e) ->
    let lset = Sv.add v lset in
    liveset_of_expr lset e
  | Eop(_, es) ->
    List.fold_left liveset_of_expr lset es
  | Eint _
  | Ebool _ -> lset

(*let liveset_of_annot lvlst =
  let lst = List.map snd lvlst in
  List.fold_left (fun s v -> Sv.add v s) Sv.empty lst*)

let liveset_of_annot lvlst =
  let filter s (t,v) =
    match v.v_ty with
    | Common.Tbase _ -> Sv.add v s
    | Common.Tarr (_, i1, i2) -> Sv.add v s
    | Common.Tmem -> assert false in
  List.fold_left filter Sv.empty lvlst

let deadcodeelim eenv m =
  let st = Ileval.find_state eenv m.mc_name in
  let eprog = st.st_eprog in
  let annot = Ileval.find_initial eenv m.mc_name in
  let liveset = ref (liveset_of_annot annot.output_var) in
  let is_livestmt instr =
    begin
      match instr.i_desc with
      | Iassgn(Lvar lv, dexpr) ->
        if Sv.mem lv !liveset then
          begin
            let lvs = Sv.remove lv !liveset in
            liveset := liveset_of_expr lvs dexpr;
            true
          end
        else
          false
      | Iassgn(Lstore(_,lv,iexpr), dexpr)
      | Iassgn(Lset(lv, iexpr), dexpr) ->
        if Sv.mem lv !liveset then
          begin
            let lvs = liveset_of_expr !liveset dexpr in
            liveset := liveset_of_expr lvs iexpr;
            true
          end
        else
          false
      | Ileak(_, dexpr) ->
        liveset := List.fold_left liveset_of_expr !liveset dexpr;
        true
      | Iigoto _ (* FIXME *)
      | Igoto _
      | Ilabel _ -> false
      | Iif _
      | Iwhile _
      | Imacro _ -> Utils.hierror "deadcodeelim" None "@[<v>cannot handle@ %a@]" (pp_i ~full:false) instr
    end in
  let elimprog = List.rev (List.filter is_livestmt (List.rev eprog)) in
  let nst = { st with st_eprog = elimprog } in
  let eenv = Ileval.update_state eenv m nst in
  eenv
