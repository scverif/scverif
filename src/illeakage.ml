open Utils
open Common
open Location
open Il
open Iltyping

let add_leakicall genv ms margs =
  let mlname = ms.mc_name ^ "_leak" in
  match find_macro_opt genv mlname with
  | None -> None
  | Some ml ->
    Some {i_desc = Imacro(ml, margs); i_loc = (ms.mc_loc, []) }

let rec traverse_code genv (instrs:Il.instr list) =
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
