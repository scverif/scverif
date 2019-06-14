open Utils
open Location

open Asmast
open Asmparser
open Asmlexer

open Ilast

let lift_param (ops:operand list) =
  ()

let lift_section (sec:section) =
  (* section becomes a macro with call statements *)
  let mloc  = loc sec in
  let m = unloc sec in
  let mname = unloc m.s_name in
  let mcalls = [] in

  let (m:Il.macro) = {
    mc_name   = mname;
    mc_id     = Uid.fresh ();
    mc_loc    = mloc;
    mc_params = [];
    mc_locals = [];
    mc_body   = mcalls
  } in
  ()

let lift (ast:section) =
  let ilast = ([], []) in
  ilast
