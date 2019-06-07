open Utils
open Location

open Asmast
open Asmparser
open Asmlexer

open Ilast
open Il
open Ilparser
open Illexer
open Ilinline

open Mainast


module Parse = struct

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

  let parse_command = fun () ->
    MenhirLib.Convert.Simplified.traditional2revised Parser.command

  let lexer lexbuf = fun () ->
    let token = Lexer.main lexbuf in
    (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

  let stdbuf = lexbuf_from_channel "stdin" stdin

  let process_command () =
    parse_command () (lexer stdbuf)

end

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

let process_il filename =
  let ilast = ILParse.process_file (Location.unloc filename) in
  let gs = Iltyping.process ilast in
  Format.eprintf "@[<v>IL definitions processed@ %a@]@."
    (pp_globals ~full:true) gs;
  let gs = Ilinline.inline_globals gs in
  Format.eprintf "@[<v>After inlining @ %a@]@."
    (pp_globals ~full:true) gs;
  ()

let process_asm filename =
  let asmsection = AsmParse.process_file (Location.unloc filename) in
  Printf.printf "%s\n%!" (Asmast.show_section asmsection);
  ()

let process_command c =
  match c with
  | Read(Asm, filename) -> process_asm filename
  | Read(Il, filename)  -> process_il filename
  | Exit -> exit 0

let main =
  while true do
    try
      Format.printf ">"; Format.print_flush ();
      let c = Parse.process_command () in
      process_command c
    with
    | Utils.HiError (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_hierror (s, loc, msg);
      exit 2
    | Utils.Error (s,loc,msg) ->
      Format.eprintf "%a@." Utils.pp_error (s, loc, msg);
  done
