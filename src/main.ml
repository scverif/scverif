(* Copyright 2019 - Inria, NXP *)

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

  let parse_scvfile = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Ilparser.scvfile

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

  let process_scvfile filename =
    let decl = from_file parse_scvfile filename in
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
    MenhirLib.Convert.Simplified.traditional2revised Asmparser.sections

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
  genv : Il.genv;
  eenv : Ileval.eenv;
}

let empty_mainenv = {
  genv = Il.empty_genv;
  eenv = Ileval.empty_eenv;
}

let process_gvar menv x =
  let genv = menv.genv in
  let x    = Iltyping.process_var_decl x in
  let genv = Iltyping.add_gvar genv x in
  Glob_option.print_normal "%a@." Il.pp_global_g (Gvar x);
  { menv with genv }

let process_macro (menv:mainenv) (m:Ilast.macro_decl located) =
  let genv = menv.genv in
  let genv = Iltyping.process_macros genv [m] in
  { menv with genv }

let process_macros (menv:mainenv) (ms:Ilast.macro_decl located list) =
  let genv = Iltyping.process_macros menv.genv ms in
  { menv with genv}

let process_verbose (menv:mainenv) (v:Scv.scvverbosity) =
  let i, _ = Scv.scvverbosity_to_glob v in
  Glob_option.set_verbose i;
  Glob_option.print_full "verbose = %i; full = %b@."
    !Glob_option.verbose !Glob_option.full;
  menv

let process_trans_partialeval (menv:mainenv) (target:Scv.scvtarget) =
  let ms = Iltyping.macronames_of_scvtarget menv.genv target in
  let partialeval eenv mn =
    let m = Iltyping.find_macro menv.genv mn in
    let estate = Ileval.partial_eval menv.genv eenv m in
    Ileval.update_state eenv mn estate in
  { menv with eenv = List.fold_left partialeval menv.eenv ms }

let process_trans_inline (menv:mainenv) (target:Scv.scvtarget) =
  let ms = Iltyping.macronames_of_scvtarget menv.genv target in
  let inline genv mn =
    let m = Iltyping.find_macro menv.genv mn in
    let m = Ilinline.inline_macro genv m in
    Iltyping.update_macro genv m in
  { menv with genv = List.fold_left inline menv.genv ms }

let process_trans_filterleakage (menv:mainenv) (target:Scv.scvtarget) (leaktarget:Scv.scvtarget) (reverse:bool) =
  let ms = Iltyping.macronames_of_scvtarget menv.genv target in
  let filterleak eenv mn =
    Illeakage.filterleakage eenv mn leaktarget reverse in
  { menv with eenv = List.fold_left filterleak menv.eenv ms }

let process_trans_addleakage (menv:mainenv) (target:Scv.scvtarget) =
  let ms = Iltyping.macronames_of_scvtarget menv.genv target in
  let addleak genv mn =
    Illeakage.addleakage genv mn in
  { menv with genv = List.fold_left addleak menv.genv ms }

let process_trans_deadcodeelim (menv:mainenv) (target:Scv.scvtarget) =
  let ms = Iltyping.macronames_of_scvtarget menv.genv target in
  let codeelim eenv m =
    let open Ilcodeelim in
      Ilcodeelim.deadcodeelim eenv m in
  { menv with eenv = List.fold_left codeelim menv.eenv ms }

let process_trans_accumulateleaks (menv:mainenv) (target:Scv.scvtarget) (leaks:Scv.scvtarget) (keep:bool) =
  let ms = Iltyping.macronames_of_scvtarget menv.genv target in
  let accumulate genv mn =
    Illeakage.accumulate_leakages genv mn leaks keep in
  { menv with genv = List.fold_left accumulate menv.genv ms }

let process_annotation (menv:mainenv) ai =
  let genv = menv.genv in
  let eenv = menv.eenv in
  let m, initial = Iltyping.process_annotation genv ai in
  let eenv = Ileval.update_initial eenv m.mc_name initial in
  { menv with eenv = eenv }

let process_check (menv:mainenv) (c:Scv.scvcmd located) =
  let mns, ca =
    (* typecheck print command and requested verbosity *)
    begin
      match unloc c with
      | Check(target, ca) ->
        (Iltyping.macronames_of_scvtarget menv.genv target, ca)
      | e ->
        Utils.hierror "Main.process_check:" (Some (loc c))
          "expected Check command but got %a" Scv.pp_scvcmd e
    end in
  List.iter
    (fun mn ->
       (* TODO fail with location in error message *)
       let m = Iltyping.find_macro menv.genv mn in
       let st = Ileval.find_state menv.eenv mn in
       let an = Ileval.find_initial menv.eenv mn in
       Ilmaskverif.check_mvprog ca m an st)
    mns;
  menv

let process_print menv (p:Scv.scvcmd located) =
  let open Scv in
  let o', f' = !Glob_option.verbose, !Glob_option.full in
  let mn, pk, v, f =
    (* typecheck print command and requested verbosity *)
    begin
      match unloc p with
      | Print(target, pk, None) ->
        (Iltyping.macronames_of_scvtarget
           menv.genv target, pk, !Glob_option.verbose, !Glob_option.full)
      | Print(target, pk, Some v) ->
        let v, f = scvverbosity_to_glob v in
        (Iltyping.macronames_of_scvtarget menv.genv target, pk, v, f)
      | e ->
        Utils.hierror "Main.process_print:" (Some (loc p))
          "expected Print command but got %a" pp_scvcmd e
    end in
  if v <= Glob_option.v_silent then
    menv
  else
    let process_pk pk =
      begin
        match pk with
        | PMacro ->
          List.iter
            (fun mn ->
               let m = Iltyping.find_macro menv.genv mn in
               Format.printf "@[<v>%a@]@."
                 (pp_macro ~full:f) m)
            mn
        | PGenv ->
          Format.printf "@[%a@]@." pp_genv_g menv.genv
        | PState ->
          List.iter
            (fun mn ->
               (* TODO fail with location in error message *)
               let st = Ileval.find_state menv.eenv mn in
               Format.printf "@[<v>state of %s:@   @[<v>%a@]@]@."
                 mn Ileval.pp_state st)
            mn
        | PInitialEnvironment ->
          List.iter
            (fun mn ->
               (* TODO fail with location in error message *)
               let i = Ileval.find_initial menv.eenv mn in
               Format.printf "@[<v>initials of %s:@   @[<v>%a@]@]@."
                 mn Ileval.pp_initial i)
            mn
        | PEvaluatedTrace ->
          List.iter
            (fun mn ->
               (* TODO fail with location in error message *)
               let st = Ileval.find_state menv.eenv mn in
               Format.printf "@[<v>evaluated trace of %s:@   @[<v>%a@]@]@."
                 mn pp_cmd_g st.st_eprog)
            mn
        | PMaskverifProg ->
          List.iter
            (fun mn ->
               (* TODO fail with location in error message *)
               let m = Iltyping.find_macro menv.genv mn in
               let st = Ileval.find_state menv.eenv mn in
               let an = Ileval.find_initial menv.eenv mn in
               Ilexport.serialize_mvprog st an m)
            mn
      end in
    Glob_option.set_verbose v;
    Glob_option.set_full f;
    process_pk pk;
    Glob_option.set_verbose o';
    Glob_option.set_full f';
    menv

let process_scvcommand mainenv (scvs:(Scv.scvval located) list) =
  let scvcmds = List.map Scv.scvval_to_scvcmd_loc scvs in
  let process_scvcmd menv cmd =
    match unloc cmd with
    | Scv.Print _ -> process_print menv cmd
    | Scv.Check _ -> process_check menv cmd
    | Scv.Accumulate(target, leaktarget, keep) ->
      process_trans_accumulateleaks menv target leaktarget keep
    | Scv.AddLeakCalls(target) ->
      process_trans_addleakage menv target
    | Scv.DeadCodeElim(target) ->
      process_trans_deadcodeelim menv target
    | Scv.PartialEval(target) ->
      process_trans_partialeval menv target
    | Scv.FilterLeakage(target, leaktarget, reverse) ->
      process_trans_filterleakage menv target leaktarget reverse
    | Scv.InlineMacros(target) ->
      process_trans_inline menv target
    | Scv.Verbosity(verbosity) ->
      process_verbose menv verbosity
  in
  List.fold_left process_scvcmd mainenv scvcmds

let rec process_ilcommand really_exit mainenv = function
  | Ilast.Gvar x -> process_gvar mainenv x
  | Ilast.Gmacro m -> process_macro mainenv m
  | Ilast.Gannotation evi -> process_annotation mainenv evi
  | Ilast.Ginclude(Asm, filename) -> process_asm mainenv filename
  | Ilast.Ginclude(Il, filename) -> process_il mainenv filename
  | Ilast.Gscvcmd(scvs) -> process_scvcommand mainenv scvs
  | Ilast.Gexit -> if really_exit then exit 0 else mainenv

and process_asm mainenv filename =
  let asmasts = AsmParse.process_file (Location.unloc filename) in
  Glob_option.print_full "@[<v>ASM program parsed@ %a@]@."
    (pp_list "@ @ " Asmast.pp_section) asmasts;
  let ilasts = List.map Asmlifter.lift_section asmasts in
  process_macros mainenv ilasts

and process_il mainenv filename =
  let ilast = ILParse.process_file (Location.unloc filename) in
  List.fold_left (process_ilcommand false) mainenv ilast

and process_scv mainenv filename =
  let scv = ILParse.process_scvfile (Location.unloc filename) in
  process_scvcommand mainenv scv

let main =
  let mainenv = ref empty_mainenv in
  let interactive = fun () ->
    while true do
      try
        Format.print_flush ();
        let c = ILParse.process_command () in
        mainenv := process_ilcommand true !mainenv c
      with
      | Utils.HiError (s,loc,msg) ->
        Format.eprintf "%a@." Utils.pp_hierror (s, loc, msg);
        exit 2
      | Utils.Error (s,loc,msg) ->
        Format.eprintf "%a@." Utils.pp_error (s, loc, msg);
    done in
  let cmdloc fname = Location.mk_loc
      { Location._dummy with
        loc_fname = "Command-line argument";
        loc_start = (!Arg.current, 0) }
      fname in
  let process_dbg' i =
    (Glob_option.set_verbose i; Format.print_flush ()) in
  let process_asm' fname =
    (mainenv := process_asm !mainenv (cmdloc fname); Format.print_flush ()) in
  let process_il' fname =
    (mainenv := process_il !mainenv (cmdloc fname); Format.print_flush ()) in
  let process_scv' fname =
    (mainenv := process_scv !mainenv (cmdloc fname); Format.print_flush ()) in
  let argspec =
    [("--asm", Arg.String process_asm', ": process code in an assembly file");
     ("--il", Arg.String process_il', ": process declarations in an il file");
     ("--scv", Arg.String process_scv', ": process commands in a scv file");
     ("-v", Arg.Int process_dbg', ": set verbosity");
     ("-i", Arg.Unit interactive, ": interactive mode");] in
  let cmdlineusage =
    "usage: " ^ Sys.argv.(0) ^
    " [--asm filename] [--il filename] [--scv filename] [-v level] [-i]" in
  let batch = fun () ->
    (* process command line arguments in order *)
    try
      Arg.parse argspec (fun a -> raise (Arg.Bad ("Parameter unknown " ^ a))) cmdlineusage;
    with
    | Utils.HiError (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_hierror (s, loc, msg);
      exit 2
    | Utils.Error (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_error (s, loc, msg);
      exit 2 in
  batch ()
