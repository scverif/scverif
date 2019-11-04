(* Copyright 2019 - NXP *)

open Utils
open Common
open Location
open Il
open Iltyping
open Ileval
open Maskverif

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

let mv_pp_header fmt (an, undefvar) =
  begin
    [@warning "-26"]
    let filter ty (t,v) ls =
      if ty == t then
        v::ls
      else
        ls in
    let pub_of l = List.fold_right (filter Ileval.Public) l [] in
    let sec_of l = List.fold_right (filter Ileval.Secret) l [] in
    let sha_of l = List.fold_right (filter Ileval.Sharing) l [] in
    let rnd_of l = List.fold_right (filter Ileval.URandom) l [] in
    (* FIXME/TODO secrets need to be understood by maskverif or used for CT-type-checking *)
    Format.fprintf fmt "@[<v>public inputs: %a@ input shares: %a@ output shares: %a@ randoms: %a@ others: %a;@]"
      (pp_list ", " mv_pphdr_var) (pub_of an.input_var)
      (pp_list ", " mv_pphdr_var) (sha_of an.input_var)
      (pp_list ", " mv_pphdr_var) (sha_of an.output_var)
      (pp_list ", " mv_pphdr_var) (rnd_of an.input_var)
      (pp_list ", " mv_pphdr_var) undefvar
  end

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

let print_mv params st an m =
  let undefvars =
    let anvar = List.map snd (an.output_var @ an.input_var) in
    let var_of_assgn_not_in_env vs (i:Il.instr) =
      match i.i_desc with
      | Il.Iassgn (Il.Lvar v, _)
      | Il.Iassgn (Il.Lset (v, _), _) ->
        if List.mem v vs || List.mem v anvar then
          vs
        else
          [v]@vs
      | Il.Iassgn (Il.Lstore (_, _, _), _) ->
        Utils.hierror "Ilexport.print_mv" (Some (fst i.i_loc))
          "@[Expecting evaluated program, cannot handle memory %a]"
          (pp_i ~full:!Glob_option.full) i
      | Il.Ileak (_, _)
      | Il.Ilabel _ -> vs
      | Il.Imacro (_, _)
      | Il.Igoto _
      | Il.Iigoto _
      | Il.Iwhile (_, _, _)
      | Il.Iif (_, _, _) ->
        Utils.hierror "Ilexport.print_mv" (Some (fst i.i_loc))
          "@[Expecting evaluated program, cannot handle %a]" (pp_i ~full:!Glob_option.full) i
      in
    List.fold_left var_of_assgn_not_in_env [] st.st_eprog
  in
  Format.printf "@[<v>proc %s:@   %a@ @ %a@ end@ para noglitch %a %s@ @]@."
    m.mc_name
    mv_pp_header (an, undefvars)
    mv_pp_body st.st_eprog
    Scv.pp_scvcheckkind params
    m.mc_name

let check_maskverif params st an m =
  false
