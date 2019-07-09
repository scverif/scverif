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
  Format.fprintf fmt "@[<v>public inputs: %a@ input shares: %a@ output shares: %a@ randoms: %a;@]"
    (pp_list ", " mv_pp_var) (pub_of an.input_var)
    (pp_list ", " mv_pp_var) (sha_of an.input_var)
    (pp_list ", " mv_pp_var) (sha_of an.output_var)
    (pp_list ", " mv_pp_var) (rnd_of an.input_var)

let mv_pp_body fmt tr =
  Format.fprintf fmt "@[<v>  @[<v>%a@]@]"
    (pp_list "@ " (pp_i ~full:!Glob_option.full)) tr

let print_mv st an m =
  Format.printf "@[<v>proc %s:@   %a@ @ %a@ end@ @]"
    m.mc_name
    mv_pp_header an
    mv_pp_body st.st_eprog
