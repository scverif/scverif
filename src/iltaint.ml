(* Copyright 2020 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)
open Utils
open Common
open Ileval
open Il

let tname = "Iltaint"

module RD = struct
  type t = rdecl
  let compare = compare
end

module Srd = Set.Make(RD)

type ienv = {
  mutable racc : Sv.t; (** variables which need an input annotation *)
  mutable wacc : Sv.t; (** variables which are assigned and might need
                           an output annotation *)
}

let empty_ienv = { racc = Sv.empty; wacc = Sv.empty; }

let addread (e:ienv) (v:Il.var) : unit =
  if not (Sv.mem v e.wacc) then
    e.racc <- Sv.add v e.racc

let warn_incomplete = Glob_option.eprint_normal
    "%s: trace contains global calls or jumps, inference of taints potentially incomplete!"
    tname

let rec infer_io_e (env:ienv) (e:expr)
  : unit =
  match e with
  | Il.Eint _
  | Il.Ebool _ -> ()
  | Il.Evar v -> addread env v
  | Il.Eget (v, e)
  | Il.Eload (_, v, e) -> addread env v; infer_io_e env e
  | Il.Eop (_, es) -> List.iter (infer_io_e env) es

let rec infer_io_i (env:ienv) (i:Il.instr) : unit =
  match i.i_desc with
  | Iassgn(Lvar v, e) ->
    infer_io_e env e;
    env.wacc <- Sv.add v env.wacc
  | Iassgn(Lset(v, ie), e) ->
    infer_io_e env ie;
    infer_io_e env e;
    env.wacc <- Sv.add v env.wacc
  | Iassgn(Lstore(_, v, em), e) ->
    infer_io_e env em;
    infer_io_e env e;
    env.wacc <- Sv.add v env.wacc
  | Ileak(_, es) ->
    List.iter (infer_io_e env) es
  | Iigoto v -> warn_incomplete; addread env v
  | Imacro(_, ps) -> warn_incomplete;
    List.iter
      (function
        | Aexpr e -> infer_io_e env e
        | Alabel _ -> ()
        | Aindex(v, _, _) -> addread env v)
      ps
  | Ilabel _
  | Igoto _ -> ()
  | Iif(e, c1, c2) ->
    infer_io_e env e;
    List.iter (infer_io_i env) c1;
    List.iter (infer_io_i env) c2
  | Iwhile(c1, e, c2) ->
    List.iter (infer_io_i env) c1;
    infer_io_e env e;
    List.iter (infer_io_i env) c2

let infer_io_vars cmd : ienv =
  let env = empty_ienv in
  List.iter (infer_io_i env) cmd;
  env

(** infer the annotation for non-annotated variables in a given program *)
let infertaints (genv:genv) (eenv:eenv) (mn:macro_name) (params:Scv.scvtaintparam)
  : eenv =
  Glob_option.print_full
    "@[executing infertaints with params:@    @[<v>%a@]@]@."
    Scv.pp_scvtaintparam params;
  let initials = Ileval.find_initial eenv mn in
  let state = Ileval.find_state eenv mn in

  (* lift IL variable to a resource declaration (Ileval.rdecl) list *)
  let v2rds (v:var) : Ileval.rdecl list =
    match v.v_ty with
    | Tbase _ -> [RDvar v]
    | Tarr(_, i1, i2) ->
      let size = B.to_int (B.sub i2 i1) + 1 in
      List.init size (fun i -> RDget(v, B.add i1 (B.of_int i)))
    | Tmem -> [] (* memory has been rewritten by partial evaluation *)
  in

  (* helper to extract annotated variables from input and output annotations *)
  let init_set rds =
    List.fold_left (fun s (_, _, rds) ->
        Array.fold_left (fun s rd -> Srd.add rd s) s rds) Srd.empty rds in

  (* sets of variables which have been annotated by the user *)
  let s_input  : Srd.t = init_set initials.input_var in
  let s_output : Srd.t = init_set initials.output_var in

  (* variables declared as memory regions *)
  let s_mem : Srd.t = Srd.of_list
      (List.fold_right (fun r l -> v2rds r.r_dest @ l)
         initials.init_region []) in

  (* list of global variables as Ileval.rdecl *)
  let s_glob : Srd.t = Srd.of_list
      (Ms.fold (fun _ v l -> v2rds v @ l) genv.glob_var []) in

  (* helper to add variables which have no taint *)
  let mk_extra s l =
    Array.of_list (List.filter (fun rd -> not (Srd.mem rd s)) l) in

  (* compute input and output variables from trace *)
  let env = infer_io_vars state.st_eprog in

  let prog_ivars = List.fold_right
      (fun v l -> v2rds v @ l) (Sv.to_list env.racc) [] in
  let prog_ovars = List.fold_right
      (fun v l -> v2rds v @ l) (Sv.to_list env.wacc) [] in

  (* dummy variable holding all input or output state *)
  let state_var = V.fresh "state" Location._dummy tint in

  (* construct the new input and output annotation *)
  let input_var =
    let prog_ivars = List.filter
        (fun rd -> not(Srd.mem rd s_input)) prog_ivars in

    if params.assumeInputsArePublic then
      let extra_i = mk_extra s_input prog_ivars in
      (Public, state_var, extra_i) :: initials.input_var
    else
      begin
        if not (List.is_empty prog_ivars) then
          Glob_option.eprint_silent
            "Missing annotation in %s for variable:@   @[<v>%a@]@ @ annotations:@   @[<v>%a@]@."
            mn (pp_list ", " Ileval.pp_rdecl) prog_ivars
            (pp_list ", " Ileval.pp_rdecl) (Srd.to_list s_input);
          initials.input_var
      end
  in

  let output_var =
    match params.taintOutputsAsPublic, params.taintMemoryOutputAsPublic with
    | true, true ->
      (* only variables which are globally accessible are relevant outputs
         (i.e. locals are not) *)
      let extra_o = Array.of_list
          (List.filter (fun rd -> not (Srd.mem rd s_output)
                                  && Srd.mem rd s_glob) prog_ovars) in
      (Public, state_var, extra_o) :: initials.output_var
    | true, false ->
      (* exclude variables which are located in memory *)
      let set = Srd.union s_output s_mem in
      let extra_o = Array.of_list
          (List.filter (fun rd -> not (Srd.mem rd set)
                                  && Srd.mem rd s_glob) prog_ovars) in
      (Public, state_var, extra_o) :: initials.output_var
    | false, true ->
      (* this makes very little sense but is supported *)
      let extra_o = Array.of_list
          (List.filter (fun rd -> not (Srd.mem rd s_output)
                                  && Srd.mem rd s_mem) prog_ovars) in
      (Public, state_var, extra_o) :: initials.output_var
    | false, false ->
      initials.output_var in

  let initials = { initials with input_var; output_var; } in

  Ileval.update_initial eenv mn initials
