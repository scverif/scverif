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

(* tweaked serialization from Il.ml *)
let rec mv_pp_e ~full fmt e =
  match e with
  | Eint i  ->
    let v = B.to_zint i in
    if Z.fits_int32 v then
      Format.fprintf fmt "(0x%s:w32)" (Z.format "02x" v)
  | Ebool b -> Format.fprintf fmt "(0b%b:w1)" b
  | Evar v  -> V.pp_full ~full fmt v
  | Eget(x,e) ->
    Format.fprintf fmt "%a[%a]" (V.pp_full ~full) x (pp_e ~full) e (* INT allowed here *)
  | Eload(ws, x, e) ->
    assert false
  | Eop(op, es) ->
    match op.od, es with
    | Oif _, _     -> assert false
    | Oadd ws, es  -> mv_pp_op2_ows ~full "#add" fmt ws es
    | Omul ws, es  -> mv_pp_op2_ows ~full "*" fmt ws es
    | Omulh ws, es -> mv_pp_op2_ws ~full "**" fmt ws es
    | Osub ws, es  -> mv_pp_op2_ows ~full "-" fmt ws es
    | Oopp ws, es  -> mv_pp_op1_ows ~full "-" fmt ws es
    | Olsl ws, es  -> mv_pp_op2_ws  ~full "<<" fmt ws es
    | Olsr ws, es  -> mv_pp_op2_ws  ~full ">>" fmt ws es
    | Oasr ws, es  -> mv_pp_op2_ws  ~full ">>s" fmt ws es
    | Oand ws, es  -> mv_pp_op2_ows ~full "&" fmt ws es
    | Oxor ws, es  -> mv_pp_op2_ows ~full "^" fmt ws es
    | Oor  ws, es  -> mv_pp_op2_ows ~full "|" fmt ws es
    | Onot ws, es  -> mv_pp_op1_ows ~full "!" fmt ws es
    | Oeq  ws, es  -> mv_pp_op2     ~full "==" fmt es
    | Olt(s,ws),es ->
      let s = if s = Signed then "<s" else "<" in
      mv_pp_op2_ows ~full s fmt ws es
    | Ole(s,ws),es ->
      let s = if s = Signed then "<=s" else "<=" in
      mv_pp_op2_ows ~full s fmt ws es
    | Osignextend(_, ws), es ->
      mv_pp_op1_ws ~full "signextend" fmt ws es
    | Ozeroextend(_, ws), es ->
      mv_pp_op1_ws ~full "zeroextend" fmt ws es
    | Ocast_int(s,_), es ->
      let s = if s = Signed then "(int)" else "(uint)" in
      mv_pp_op1 ~full s fmt es
    | Ocast_w ws, es ->
      let s = Format.sprintf "(%s)" (ws_string ws) in
      mv_pp_op1 ~full s fmt es

and mv_pp_be ~full fmt e =
  match e with
  | Eint _ | Ebool _ | Evar _ | Eget _ | Eload _ -> mv_pp_e ~full fmt e
  | Eop _ -> Format.fprintf fmt "(%a)" (mv_pp_e ~full) e

and mv_pp_op2 ~full s fmt es =
  match es with
  | [e1; e2] ->
    Format.fprintf fmt "@[%a %s@ %a@]" (mv_pp_be ~full) e1 s (mv_pp_be ~full) e2
  | _ -> assert false

and mv_pp_op2_ws ~full s fmt ws es =
  let s = Format.sprintf "%s%s" s (ws_string ws) in
  mv_pp_op2 ~full s fmt es

and mv_pp_op2_ows ~full s fmt ws es =
  match ws with
  | None -> Format.printf "shit@.";mv_pp_op2 ~full s fmt es
  | Some ws -> mv_pp_op2_ws ~full s fmt ws es

and mv_pp_op1 ~full s fmt es =
  match es with
  | [e1] ->
    Format.fprintf fmt "@[%s@ %a@]" s (mv_pp_be ~full) e1
  | _ -> assert false

and mv_pp_op1_ws ~full s fmt ws es =
  let s = Format.sprintf "%s%s" s (ws_string ws) in
  mv_pp_op1 ~full s fmt es

and mv_pp_op1_ows ~full s fmt ws es =
  match ws with
  | None -> mv_pp_op1 ~full s fmt es
  | Some ws -> mv_pp_op1_ws ~full s fmt ws es

let mv_pp_lval ~full fmt lv =
  match lv with
  | Lvar x ->
    Format.fprintf fmt "%a" (V.pp_full ~full) x
  | Lset(x, e) ->
    Format.fprintf fmt "%a[%a]" (V.pp_full ~full) x (pp_e ~full) e (* integers allowed *)
  | Lstore(_, _, _) ->
    assert false

let mv_pp_marg ~full fmt = function
  | Aexpr e -> mv_pp_e ~full fmt e
  | Alabel lbl -> Lbl.pp_full ~full fmt lbl
  | Aindex (x,i1,i2) ->
    Format.fprintf fmt "%a[%a:%a]"
      (V.pp_full ~full) x B.pp_print i1 B.pp_print i2

let mv_pp_margs ~full fmt args =
  Format.fprintf fmt "@[(%a)@]"
    (pp_list ",@ " (mv_pp_marg ~full)) args

let rec mv_pp_i ~full fmt i =
  match i.i_desc with
  | Iassgn(x,e) ->
    Format.fprintf fmt "@[%a <-@ %a;@]"
      (mv_pp_lval ~full) x (mv_pp_e ~full) e
  | Il.Ileak(li, es) ->
    Format.fprintf fmt "@[leak %a (%a) @[<v>\"%a(%a)@,in %a\"@];@]"
      pp_leak_info li (pp_list ", " (mv_pp_e ~full:!Glob_option.full)) es
      pp_leak_info li (pp_list ", " (mv_pp_e ~full:!Glob_option.full)) es
      pp_full_loc_first i.i_loc
  | Il.Ilabel l ->
    Format.fprintf fmt "@[(* %a *)@]" Lbl.pp_g l
  | Imacro(mcname, args) ->
    if full then
      Format.fprintf fmt "@[%s%a;@]"
        mcname (mv_pp_margs ~full) args
    else
      Format.fprintf fmt "@[%s%a;@]"
        mcname (mv_pp_margs ~full) args
  | Igoto _
  | Iigoto _
  | Iif(_,_,_)
  | Iwhile(_, _, _) ->
    assert false

and mv_pp_cmd ~full fmt c =
  Format.fprintf fmt "@[<v>{@   @[<v>%a@]@ }@]"
    (pp_list "@ " (mv_pp_i ~full)) c

and mv_pp_else ~full fmt c =
  if c == [] then ()
  else
    Format.fprintf fmt "@ else@ %a" (mv_pp_cmd ~full) c

let mv_pp_body fmt tr =
  Format.fprintf fmt "@[<v>  @[<v>%a@]@]"
    (pp_list "@ " (mv_pp_i ~full:!Glob_option.full)) tr

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

let serialize_mvcheck params st an m =
  let undefvars = undecl_of_eprog an st in
  Format.printf "@[<v>proc %s:@   %a@ @ %a@ end@ para noglitch %a %s@ @]@."
    m.mc_name
    mv_pp_header (an, undefvars)
    mv_pp_body st.st_eprog
    Scv.pp_scvcheckkind params
    m.mc_name

let serialize_mvprog st an m =
  let undefvars = undecl_of_eprog an st in
  Format.printf "@[<v>proc %s:@   %a@ @ %a@ end@]@."
    m.mc_name
    mv_pp_header (an, undefvars)
    mv_pp_body st.st_eprog
