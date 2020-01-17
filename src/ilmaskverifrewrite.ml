(* Copyright 2020 - NXP *)
open Utils
open Ileval
open Il

let mr_error a = Utils.hierror "rewriteformaskverif" a
(*let mr_debug a : unit = Format.printf "rewriteformaskverif:@[<v>%a@]@." a *)

type inoutan =
  | Input
  | Output

let inty = Input
let outty = Output
let isinput = function | Input -> true | _ -> false
let isoutput = function | Output -> true | _ -> false
let pp_inoutan fmt io =
  match io with
  | Input ->
    Format.fprintf fmt "input"
  | Output ->
    Format.fprintf fmt "output"

type accesskind =
  | Read
  | Write

let rak = Read
let wak = Write
let iswrite = function | Write -> true | _ -> false
let isread = function | Read -> true | _ -> false
let pp_accesskind fmt io =
  match io with
  | Read ->
    Format.fprintf fmt "read"
  | Write ->
    Format.fprintf fmt "write"

type mrenv = {
  mr_m  : macro;
  mr_st : state;
  mr_inferpubin : bool;
  mr_inferstout : bool;
  mr_gv : Sv.t; (** set of global variables *)
  mr_ty : (t_ty * inoutan) Mv.t; (** annotation type of variables *)
  mr_sv : var Mv.t; (** substitute variable by variable *)
  mr_so : var Mv.t; (** potential output variable *)
}

let mki l i = {i_desc = i; i_loc = l}

(** rewrite code and produce complete annotations (header) for maskverif
    annotation policy: (NA=non-annotated)
      - do not alter existing annotations except split in/out of same variable into two variables
      - NA local variables are made public inputs to reduce code discrepancy to original input
      - NA global variables with assignments are ensured to be public after execution
    rewriting:
      - variables which are input and output become
        - input (never assigned)
        - (new) output (used on-wards)
    conditionally annotate:
      - NA variables with read access are required to be public (NA becomes public)
      - all non-annotated variables with assignments are ensured to be public after execution
        (corresponds to stateful composition)
    consistency check:
      - all variables are annotated
      - no variable has annotations both in input and output
      - no output variable is read prior assignment
*)

let findvarname (e:mrenv) (ioty:inoutan) (v:var) : string * int =
  let vn =
    if isoutput ioty then
      v.v_name ^ "out"
    else
      v.v_name ^ "in" in
    (* find the next free name *)
    let rec lkp i =
      if Mv.exists (fun v _ -> String.equal v.v_name (vn ^ (string_of_int i))) e.mr_ty then
        lkp (i+1)
      else i in
  vn, (lkp 0)

let convertvarname (s,i) =
  if i = 0 then
    s
  else
    s ^ (string_of_int i)

let split_var (e:mrenv) (ty:t_ty) (ioty:inoutan) (v:var) : mrenv * var=
  let vn = findvarname e ioty v |> convertvarname  in
  let v' = V.fresh vn v.v_loc v.v_ty in
  (* add to substitution set *)
  let mr_sv = Mv.add v v' e.mr_sv in
  let mr_ty = Mv.add v' (ty, ioty) e.mr_ty in
  Glob_option.print_full "@[rewriteformaskverif: splitting %a and (new) %a@]@."
    V.pp_dbg v V.pp_dbg v';
  { e with mr_sv; mr_ty }, v'

let getorannotate_v (i:instr) (ak:accesskind) (env:mrenv) (vi:var)
  : mrenv * var =
  (* rewrite the variable in case this is possible *)
  let v = Mv.find_default vi vi env.mr_sv in
  (* check if the variable is already annotated *)
  if Mv.mem v env.mr_ty then
    begin
      (* get the annotation *)
      let aty, inout = Mv.find v env.mr_ty in
      if (isread ak  (* read of annotated input or output is ok *)
          || (iswrite ak && isoutput inout)) (* write to annotated output is ok *)
      then
        (Glob_option.print_full
           "@[rewriteformv: found annotation %a %a for variable %a (respective %a)@]@."
           pp_t_ty aty pp_inoutan inout V.pp_g v V.pp_g vi;
         env, v)
      else if isinput inout && env.mr_inferstout then
        (* write access to input variable needs to be rewritten *)
        begin
          let env, vnew = split_var env aty outty v in
          Glob_option.print_full
           "@[rewriteformv: adding output annotation %a %a \
            for new variable %a (previously %a) due to write access.@]@."
           pp_t_ty aty pp_inoutan outty V.pp_g vnew V.pp_g v;
          env, vnew
        end
      else (* not captured *)
        mr_error (Some (fst i.i_loc))
          "@[rewriteformv: found unsuitable annotation %a %a, \
           during %a access for variable %a (%a)@]@."
          pp_t_ty aty pp_inoutan inout pp_accesskind ak V.pp_dbg v V.pp_dbg vi
    end
  else (* variable is not yet annotated *)
  if List.mem (Pvar v) env.mr_m.mc_locals then
    (* local variables are by definition public *)
    (Glob_option.print_full
       "@[rewriteformv: not annotation local variable %a@]@."
       V.pp_g v;
     env, v)
  else
    begin
      let ty, io =
        if List.mem (Pvar v) env.mr_m.mc_params then
          if isread ak && env.mr_inferpubin then
            (* asked to make param public input *)
            (Glob_option.print_full
               "@[rewriteformv: infer public annotation for param %a@]@."
               V.pp_g v;
             Ileval.Public, inty)
          else if iswrite ak && env.mr_inferstout then
            (* asked to make param public output *)
            (Glob_option.print_full
               "@[rewriteformv: infer public annotation for param %a@]@."
               V.pp_g v;
             Ileval.Public, outty)
          else
            mr_error (Some (fst i.i_loc)) "@[missing annotation for parameter %a of %s@]@."
              V.pp_dbg v env.mr_m.mc_name
        else if Sv.mem v env.mr_gv then
          if isread ak && env.mr_inferpubin then
            (* asked to make global variable public input *)
            (Glob_option.print_full
               "@[rewriteformv: infer public annotation for global var %a@]@."
               V.pp_g v;
             Ileval.Public, inty)
          else if iswrite ak && env.mr_inferstout then
            (* asked to make global variable public output *)
            (Glob_option.print_full
               "@[rewriteformv: infer public annotation for global var %a@]@."
               V.pp_g v;
             Ileval.Public, outty)
          else
            mr_error (Some (fst i.i_loc))
              "@[missing annotation for global variable access %a in %s@]"
              V.pp_dbg v env.mr_m.mc_name
        else
          mr_error (Some (fst i.i_loc))
            "@[missing annotation for unexpected %a of type %a in %s: %a@]"
            V.pp_dbg v Common.pp_ty v.v_ty env.mr_m.mc_name pp_i_dbg i
      in
      let v' = Mv.find_default v v env.mr_sv in
      {env with mr_ty = Mv.add v' (ty, io) env.mr_ty}, v'
    end

(* core expression rewrite and annotation engine *)
let rec rewrite_e (i:instr) (env:mrenv) (e:expr)
  : mrenv * expr =
  match e with
  | Il.Eint _ -> env, e
  | Il.Ebool _ -> env, e
  | Il.Evar v ->
    let env, v' = getorannotate_v i rak env v in
    env, Evar v'
  | Il.Eget (v, ei) ->
    let env, v' = getorannotate_v i rak env v in
    let env, ei' = rewrite_e i env ei in
    env, Eget(v', ei')
  | Il.Eop (o, es) ->
    let env, es = List.fold_left_map (rewrite_e i) env es in
    env, Eop(o, es)
  | Il.Eload (_, _, _) ->
   mr_error (Some (fst i.i_loc))
      "@[<v>expecting evaluated program, cannot handle load in %a@ @]"
      pp_i_dbg i

(* core code rewrite and annotation engine *)
let rewrite_i (env:mrenv) (i:instr)
  : mrenv * instr =
  Glob_option.print_full "@[rewriting instruction %a@]@." pp_i_dbg i;
  let l = i.i_loc in
  match i.i_desc with
  | Il.Iassgn (Lvar d, e) ->
    (* rewrite and annotate the rhs expression *)
    let env, e' = rewrite_e i env e in
    (* rewrite and annotate the destination variable *)
    let env, d' = getorannotate_v i wak env d in (* gets out type if not yet annotated *)
    env, Iassgn(Lvar d',e') |> mki l
  | Il.Iassgn (Lset(d, ei), e) ->
    let env, ei' = rewrite_e i env ei in
    let env, e' = rewrite_e i env e in
    let env, d' = getorannotate_v i wak env d in (* gets out type if not yet annotated *)
    env, Iassgn(Lset(d', ei), e') |> mki l
  | Il.Ileak (n, es) ->
    let env, es' = List.fold_left_map (rewrite_e i) env es in
    env, Ileak(n, es') |> mki l
  | Il.Ilabel _ -> env, i
  | Il.Iassgn(Lstore _, _)
  | Il.Imacro (_, _)
  | Il.Igoto _
  | Il.Iigoto _
  | Il.Iif (_, _, _)
  | Il.Iwhile (_, _, _) ->
    mr_error (Some (fst l))
      "@[<v>expecting evaluated program, cannot handle %a@ @]"
      pp_i_dbg i

let rewriteformaskverif (genv:genv) (eenv:eenv) (mn:macro_name) (params:Scv.scvmvrewriteparam)
  : eenv =
  Glob_option.print_full
    "@[executing rewriteformaskverif with params:@    @[<v>%a@]@]@."
    Scv.pp_scvmvrewriteparam params;
  let m = Iltyping.find_macro genv mn in
  let initials = Ileval.find_initial eenv m.mc_name in
  let st =
    try Ileval.find_state eenv m.mc_name
    with HiError (_,_,_) ->
      mr_error None
        "macro %s has not been partially evaluated yet." m.mc_name in
  let addorsplit (ioty:inoutan) (ty, v) e : mrenv =
    if Mv.exists (fun v' _ -> String.equal v.v_name v'.v_name) e.mr_ty then
      (* need to split (clone) this variable with a new name *)
      fst (split_var e ty ioty v)
    else
      { e with mr_ty = Mv.add v (ty, ioty) e.mr_ty } in
  let mrenv = {
    mr_m  = m;
    mr_st = st;
    mr_inferpubin = params.inferpubin;
    mr_inferstout = params.inferstout;
    mr_gv = Ms.fold (fun _ v m -> Sv.add v m) genv.glob_var Sv.empty;
    mr_ty = Mv.empty;
    mr_sv = Mv.empty;
    mr_so = Mv.empty;
    } |>
    (* convert list annotations to map, split variables with double annotation *)
    List.fold_right (addorsplit inty) initials.input_var |>
    List.fold_right (addorsplit outty) initials.output_var
  in
  Glob_option.print_full
    "@[rewriteformaskverif starting core rewrite@]@.";
  (* rewrite and annotate the instructions *)
  let mrenv, prog = List.fold_left_map rewrite_i mrenv st.st_eprog in
  (* recreate list style annotation *)
  let input_var, output_var =
    Mv.fold
      (fun v (ty, io) (is,os) ->
         if io == inty then (ty, v)::is,os else is,(ty, v)::os)
      mrenv.mr_ty ([],[]) in
  let input_var = List.rev input_var in
  let output_var = List.rev output_var in
  let initials = { initials with input_var; output_var } in
  let eenv = Ileval.update_initial eenv m.mc_name initials in
  let nst = { st with st_eprog = prog } in
  let eenv = Ileval.update_state eenv m.mc_name nst in
  eenv
