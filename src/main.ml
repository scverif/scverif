open Utils
open Location

open Asmast
open Asmparser
open Asmlexer
open Asmlifter

open Ilast
open Il
open Ilparser
open Illexer
open Ileval

module ILParse = struct
  module L = Lexing

  let lexbuf_from_channel = fun name channel ->
    let lexbuf = L.from_channel channel in
    lexbuf.L.lex_curr_p <- {
        L.pos_fname = name;
        L.pos_lnum  = 1;
        L.pos_bol   = 0;
        L.pos_cnum  = 0
      };
    lexbuf

  let parse_file = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Ilparser.file

  let lexer lexbuf = fun () ->
    let token = Illexer.main lexbuf in
    (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

  let from_channel parse ~name channel =
    let lexbuf = lexbuf_from_channel name channel in
    parse () (lexer lexbuf)

  let from_file parse filename =
    let channel = open_in filename in
    finally
      (fun () -> close_in channel)
      (from_channel parse ~name:filename) channel

  let process_file filename =
    let decl = from_file parse_file filename in
    decl

  let parse_command = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Ilparser.command

  let stdbuf = lexbuf_from_channel "stdin" stdin

  let process_command () =
    parse_command () (lexer stdbuf)

end

module AsmParse = struct
  module L = Lexing

  let lexbuf_from_channel = fun name channel ->
    let lexbuf = L.from_channel channel in
    lexbuf.L.lex_curr_p <- {
        L.pos_fname = name;
        L.pos_lnum  = 1;
        L.pos_bol   = 0;
        L.pos_cnum  = 0
      };
    lexbuf

  let parse_file = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Asmparser.section

  let lexer lexbuf = fun () ->
    let token = Asmlexer.main lexbuf in
    (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

  let from_channel parse ~name channel =
    let lexbuf = lexbuf_from_channel name channel in
    parse () (lexer lexbuf)

  let from_file parse filename =
    let channel = open_in filename in
    finally
      (fun () -> close_in channel)
      (from_channel parse ~name:filename) channel

  let process_file filename =
    let decl = from_file parse_file filename in
    decl

end

type mainenv = {
  genv : Iltyping.genv;
  eenv : Ileval.eenv;
}

let empty_mainenv = {
  genv = Iltyping.empty_genv;
  eenv = Ileval.empty_eenv;
}

let process_gvar menv x =
  let genv = menv.genv in
  let x    = Iltyping.process_var_decl x in
  let genv = Iltyping.add_gvar genv x in
  Glob_option.print_normal "%a@." Il.pp_global_g (Gvar x);
  { menv with genv = genv }

let process_macro menv m =
  let genv = menv.genv in
  let m = Iltyping.process_macro genv m in
  Glob_option.print_full "@[<v>type checked %s@ %a@]@."
    m.mc_name Il.pp_global_g (Gmacro m);
  let genv = Iltyping.add_macro genv m in
  { menv with genv = genv }

let process_verbose menv i =
  Glob_option.set_verbose i;
  Glob_option.print_full "verbose = %i; full = %b@."
    !Glob_option.verbose !Glob_option.full;
  menv

let process_trans_eval (eenv:Ileval.eenv) ms =
  let open Ileval in
  let peval eenv m =
    let estate = Ileval.partial_eval eenv m in
    Glob_option.print_normal "@[<v>partial evaluation of %s@ %a@]@."
      m.mc_name pp_state estate;
    let eenv = Ileval.update_state eenv m estate in
    eenv in
  List.fold_left peval eenv ms

let process_trans_inline genv ms =
  let pinline genv m =
    let m = Ilinline.inline_macro m in
    Glob_option.print_normal "@[<v>%a@]@." Il.pp_global_g (Gmacro m);
    let genv = Iltyping.update_macro genv m in
    genv in
  List.fold_left pinline genv ms

let process_trans_addleakage genv ms =
  let addleak genv m =
    let open Illeakage in
    Illeakage.add_leakage genv m in
  List.fold_left addleak genv ms

let process_trans_deadcodeelim eenv ms =
  let pelim eenv m =
    let open Ilcodeelim in
      Ilcodeelim.deadcodeelim eenv m in
   List.fold_left pelim eenv ms

let process_apply_transformation menv api =
  let genv = menv.genv in
  let eenv = menv.eenv in
  let macros = Iltyping.process_apply_ms genv api in
  match unloc api.apply_t with
  | "inline" ->
    { menv with genv = process_trans_inline genv macros }
  | "addleakage" ->
    { menv with genv = process_trans_addleakage genv macros }
  | "partialeval" ->
    { menv with eenv = process_trans_eval eenv macros }
  | "deadcodeelim" ->
    (* FIXME: check availablity of eprog for m *)
    { menv with eenv = process_trans_deadcodeelim eenv macros }
  | i ->
    Utils.hierror "apply_transformation" (Some (loc api.apply_t))
      "@[<v> transformation %s unknown@]" i

let process_annotation menv ai =
  let genv = menv.genv in
  let eenv = menv.eenv in
  let m, initial = Iltyping.process_annotation genv ai in
  let eenv = Ileval.update_initial eenv m initial in
  { menv with eenv = eenv }

let process_print menv pi =
  (* FIXME: typecheck pi.p_ms *)
  let tprint menv pi =
    match pi.p_pk with
    | Macro ->
      begin
        match Iltyping.find_macro_opt menv.genv (unloc pi.p_id) with
        | Some m ->
          Format.printf "@[<v>%a@]@."
            (pp_macro ~full:!Glob_option.full) m
        | None ->
          Utils.hierror "process_print" (Some (loc pi.p_id))
        "@[<v> unknown macro %s@]" (unloc pi.p_id)
      end
    | State ->
      begin
        let st = Ileval.find_state menv.eenv (unloc pi.p_id) in
        Format.printf "@[<v>state of %s:@ %a@]@."
          (unloc pi.p_id) (pp_state) st
      end
    | EvalTrace ->
      begin
        let st = Ileval.find_state menv.eenv (unloc pi.p_id) in
        Format.printf "@[<v>evaluated trace of %s:@ %a]@."
          (unloc pi.p_id) (pp_cmd ~full:!Glob_option.full) st.st_eprog
      end in
  List.iter (tprint menv) pi;
  menv

let rec process_command really_exit mainenv = function
  | Ilast.Gvar x   -> process_gvar mainenv x
  | Ilast.Gmacro m -> process_macro mainenv m
  | Ilast.Gannotation evi -> process_annotation mainenv evi
  | Ilast.Gapply api -> process_apply_transformation mainenv api
  | Ilast.Ginclude (Asm, filename) -> process_asm mainenv filename
  | Ilast.Ginclude (Il, filename) -> process_il mainenv filename
  | Ilast.Gverbose i -> process_verbose mainenv i
  | Ilast.Gprint i -> process_print mainenv i
  | Ilast.Gexit    -> if really_exit then exit 0 else mainenv

and process_asm mainenv filename =
  let asmast = AsmParse.process_file (Location.unloc filename) in
  Glob_option.print_full "@[<v>ASM program parsed@ %a@]@."
    Asmast.pp_section asmast;
  let ilast = Asmlifter.lift_section asmast in
  Glob_option.print_full "@[<v>ASM lifted to IL@ %a@]@."
    Ilast.pp_command ilast;
  process_command false mainenv ilast

and process_il mainenv filename =
  let ilast = ILParse.process_file (Location.unloc filename) in
  List.fold_left (process_command false) mainenv ilast

let main =
  let mainenv = ref empty_mainenv in
  while true do
    try
    (*  Format.printf ">"; Format.print_flush (); *)
      let c = ILParse.process_command () in
      mainenv := process_command true !mainenv c
    with
    | Utils.HiError (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_hierror (s, loc, msg);
      exit 2
    | Utils.Error (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_error (s, loc, msg);
  done
