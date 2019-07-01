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

let process_gvar genv x =
  let x    = Iltyping.process_var_decl x in
  let genv = Iltyping.add_gvar genv x in
  Glob_option.print_normal "%a@." Il.pp_global_g (Gvar x);
  genv

let process_macro genv m =
  let m = Iltyping.process_macro genv m in
  Glob_option.print_full "@[<v>type checked b inlining@ %a@]@."
    Il.pp_global_g (Gmacro m);
  let genv = Iltyping.add_macro genv m in
  genv

let process_eval genv evi =
  let m, initial = Iltyping.process_eval genv evi in
  let c = partial_eval initial m in
  Glob_option.print_silent "@[<v>partial evaluation of %s@ %a@]@."
    m.mc_name pp_cmd_g c;
  genv

let process_verbose genv i =
  Glob_option.set_verbose i;
  Format.printf "verbose = %i; full = %b@."
    !Glob_option.verbose !Glob_option.full;
  genv

let process_trans_inline genv m =
  let m = Ilinline.inline_macro m in
  Glob_option.print_normal "@[<v>%a@]@." Il.pp_global_g (Gmacro m);
  let genv = Iltyping.update_macro genv m in
  genv

let process_trans_inlines genv ms =
  List.fold_left process_trans_inline genv ms

let process_trans_addleakage genv ms =
  let open Illeakage in
  let addleak genv m = Illeakage.add_leakage genv m in
  List.fold_left addleak genv ms

let process_apply_transformation genv api =
  let ms = Iltyping.process_apply genv api in
  match unloc api.apply_t with
  | "inline" -> process_trans_inlines genv ms
  | "addleakage" -> process_trans_addleakage genv ms
  | i ->
    Utils.hierror "apply_transformation" (Some (loc api.apply_t))
      "@[<v> transformation %s unknown@]" i

let rec process_command really_exit genv = function
  | Ilast.Gvar x   -> process_gvar genv x
  | Ilast.Gmacro m -> process_macro genv m
  | Ilast.Geval evi -> process_eval genv evi
  | Ilast.Gapply api -> process_apply_transformation genv api
  | Ilast.Ginclude (Asm, filename) -> process_asm genv filename
  | Ilast.Ginclude (Il, filename) -> process_il genv filename
  | Ilast.Gverbose i -> process_verbose genv i
  | Ilast.Gexit    -> if really_exit then exit 0 else genv

and process_asm genv filename =
  let asmast = AsmParse.process_file (Location.unloc filename) in
  Glob_option.print_full "@[<v>ASM program parsed@ %a@]@."
    Asmast.pp_section asmast;
  let ilast = Asmlifter.lift_section asmast in
  Glob_option.print_full "@[<v>ASM lifted to IL@ %a@]@."
    Ilast.pp_command ilast;
  process_command false genv ilast

and process_il genv filename =
  let ilast = ILParse.process_file (Location.unloc filename) in
  List.fold_left (process_command false) genv ilast

let main =
  let genv = ref Iltyping.empty_genv in
  while true do
    try
    (*  Format.printf ">"; Format.print_flush (); *)
      let c = ILParse.process_command () in
      genv := process_command true !genv c
    with
    | Utils.HiError (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_hierror (s, loc, msg);
      exit 2
    | Utils.Error (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_error (s, loc, msg);
  done
