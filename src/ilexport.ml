(* Copyright 2019 - NXP *)

open Utils
open Common
open Il
open Iltyping
open Ileval

let mv_pphdr_var fmt v =
  match v.v_ty with
  | Tarr(bty,i1,i2) ->
    Format.fprintf fmt "%a %a[%a:%a]"
      pp_bty bty
      V.pp_g v
      B.pp_print i1
      B.pp_print i2
  | Tbase bty ->
    Format.fprintf fmt "%a %a"
      pp_bty bty
      V.pp_g v
  | Tmem -> assert false

let atys_of_an aty ans =
  let filter ty (t,v) ls =
    if ty == t then
      v::ls
    else
      ls in
  List.fold_right (filter aty) ans []

let mv_pp_header fmt (an, undefvar) =
  let secvars = atys_of_an Ileval.Secret an.input_var in
  if secvars == [] then
    let pubvars = atys_of_an Ileval.Public an.input_var in
    let shaoutvars = atys_of_an Ileval.Sharing an.output_var in
    let shainvars = atys_of_an Ileval.Sharing an.input_var in
    let rndvars = atys_of_an Ileval.URandom an.input_var in
    Format.fprintf fmt
      "@[<v>public inputs: %a@ input shares: %a@ output shares: %a@ randoms: %a@ others: %a;@]"
      (pp_list ", " mv_pphdr_var) pubvars
      (pp_list ", " mv_pphdr_var) shainvars
      (pp_list ", " mv_pphdr_var) shaoutvars
      (pp_list ", " mv_pphdr_var) rndvars
      (pp_list ", " mv_pphdr_var) undefvar
  else
    Utils.hierror "Ilexport.mv_pp_header" None
      "@[Cannot serialize programs with unshared sensitive inputs: %a@]"
      (pp_list ", " V.pp_g) secvars

let mv_pp_body fmt tr =
  let pp_b fmt (i:Il.instr) =
    match i.i_desc with
    | Il.Ileak(li, es) ->
      Format.fprintf fmt "@[leak %a (%a) @[<v>\"%a(%a)@,in %a\"@];@]"
        pp_leak_info li (pp_list ", " (pp_e ~full:!Glob_option.full)) es
        pp_leak_info li (pp_list ", " (pp_e ~full:!Glob_option.full)) es
        pp_full_loc_first i.i_loc
    | _ -> Format.fprintf fmt "%a" (pp_i ~full:!Glob_option.full) i in
  Format.fprintf fmt "@[<v>  @[<v>%a@]@]"
    (pp_list "@ " pp_b) tr

let undecl_of_eprog an st =
  let anvar = List.map snd (an.output_var @ an.input_var) in
  let var_of_assgn_not_in_env vs (i:Il.instr) =
    match i.i_desc with
    | Il.Iassgn (Il.Lvar v, _)
    | Il.Iassgn (Il.Lset (v, _), _) ->
      if List.exists (fun e -> e.v_name = v.v_name ) vs || List.mem v anvar then
        vs
      else
        [v]@vs
    | Il.Iassgn (Il.Lstore (_, _, _), _) ->
      Utils.hierror "Ilexport.print_mv" (Some (fst i.i_loc))
        "@[Expecting evaluated program, cannot handle memory %a@]"
        (pp_i ~full:!Glob_option.full) i
    | Il.Ileak (_, _)
    | Il.Ilabel _ -> vs
    | Il.Imacro (_, _)
    | Il.Igoto _
    | Il.Iigoto _
    | Il.Iwhile (_, _, _)
    | Il.Iif (_, _, _) ->
      Utils.hierror "Ilexport.print_mv" (Some (fst i.i_loc))
        "@[Expecting evaluated program, cannot handle %a]"
        (pp_i ~full:!Glob_option.full) i
  in
  List.fold_left var_of_assgn_not_in_env [] st.st_eprog

let print_mv params st an m =
  let undefvars = undecl_of_eprog an st in
  Format.printf "@[<v>proc %s:@   %a@ @ %a@ end@ para noglitch %a %s@ @]@."
    m.mc_name
    mv_pp_header (an, undefvars)
    mv_pp_body st.st_eprog
    Scv.pp_scvcheckkind params
    m.mc_name
