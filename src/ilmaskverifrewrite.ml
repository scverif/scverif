(* Copyright 2020 - NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)
open Utils
open Common
open Ileval
open Il

module RD = struct
  type t = rdecl
  let compare = compare
end

module Srd = Set.Make(RD)

let rewriteformaskverif (genv:genv) (eenv:eenv) (mn:macro_name) (params:Scv.scvmvrewriteparam)
  : eenv =
  Glob_option.print_full
    "@[executing rewriteformaskverif with params:@    @[<v>%a@]@]@."
    Scv.pp_scvmvrewriteparam params;
  let m = Iltyping.find_macro genv mn in
  let initials = Ileval.find_initial eenv m.mc_name in

  let v2rds v =
    match v.v_ty with
    | Tbase _ -> [RDvar v]
    | Tarr(_,i1,i2) ->
      let size = B.to_int (B.sub i2 i1) + 1 in
      List.init size (fun i -> RDget(v,B.add i1 (B.of_int i)))
    | Tmem -> [] in
  let glob_vars = 
    Ms.fold (fun _ v gv -> v2rds v @ gv) genv.glob_var [] in

  let init_set rds = 
    List.fold_left (fun s (_,_,rds) ->
        Array.fold_left (fun s rd -> Srd.add rd s) s rds) Srd.empty rds in
 
  let s_input = init_set initials.input_var in
  let s_output = init_set initials.output_var in
  
  let mk_extra s l = 
    Array.of_list (List.filter (fun rd -> not (Srd.mem rd s)) l) in
  let extra_i = mk_extra s_input glob_vars in
  let extra_o = mk_extra s_output glob_vars in

  let dummy_var = V.fresh "dummy" Location._dummy tint in
  
  let initials = 
    {initials with
      input_var  = (Public, dummy_var, extra_i) :: initials.input_var;
      output_var = (Public, dummy_var, extra_o) :: initials.output_var;
    } in
  Ileval.update_initial eenv m.mc_name initials
