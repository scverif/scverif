open Utils
open Common
open Location
open Il
open Iltyping

let check_param_eq m ml =
  let param_eq b p1 p2 =
    match p1, p2 with
    | Pvar v1, Pvar v2 -> ty_eq v1.v_ty v2.v_ty
    | Plabel l1, Plabel l2 -> true
    | _, _ -> false in
  try
    List.fold_left2 param_eq true m.mc_params ml.mc_params
  with Invalid_argument _ ->
    false

let add_leakicall genv m margs =
  let mlname = m.mc_name ^ "_leak" in
  match find_macro_opt genv mlname with
  | None -> None
  | Some ml ->
    if check_param_eq m ml then
      Some {i_desc = Imacro(ml, margs); i_loc = (ml.mc_loc, []) }
    else
      error "add_leakicall:" (ml.mc_loc, [])
        "parameters of %s do not match %s" ml.mc_name m.mc_name

let rec traverse_code genv instrs =
  match instrs with
  | [] -> []
  | i::is ->
    begin
      match i.i_desc with
      | Imacro(m, margs) ->
        begin
          match (add_leakicall genv m margs) with
          | Some il -> il :: i :: traverse_code genv is
          | None -> i :: traverse_code genv is
        end
      | _ -> i :: traverse_code genv is
    end

let add_leakage genv m =
  let leakycmds = traverse_code genv m.mc_body in
  let ml = {m with mc_body = leakycmds } in
  let genv = Iltyping.update_macro genv ml in
  genv
