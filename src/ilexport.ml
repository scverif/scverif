open Utils
open Common
open Location
open Il
open Iltyping
open Ileval

let mv_pp_var fmt v =
  match v.v_ty with
  | Tarr(_,i1,i2) ->
    Format.fprintf fmt "%a[%a:%a]"
      V.pp_g v
      B.pp_print i1
      B.pp_print i2
  | Tbase _ ->
    Format.fprintf fmt "%a"
      V.pp_g v
  | Tmem -> assert false

let mv_pp_header fmt an =
  let filter ty (t,v) ls =
    if ty == t then
      v::ls
    else
      ls in
  let pub_of l = List.fold_right (filter Ileval.Public) l [] in
  let sec_of l = List.fold_right (filter Ileval.Secret) l []in
  let sha_of l = List.fold_right (filter Ileval.Sharing) l [] in
  let rnd_of l = List.fold_right (filter Ileval.URandom) l [] in
  (* FIXME/TODO secrets need to be understood by maskverif or used for CT-type-checking *)
  Format.fprintf fmt "@[<v>public inputs: %a@ input shares: %a@ output shares: %a@ randoms: %a;@]"
    (pp_list ", " mv_pp_var) (pub_of an.input_var)
    (pp_list ", " mv_pp_var) (sha_of an.input_var)
    (pp_list ", " mv_pp_var) (sha_of an.output_var)
    (pp_list ", " mv_pp_var) (rnd_of an.input_var)

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
  Format.printf "@[<v>proc %s:@   %a@ @ %a@ end@ para noglitch %a %s@ @]"
    m.mc_name
    mv_pp_header an
    mv_pp_body st.st_eprog
    Scv.pp_scvcheckkind params
    m.mc_name
