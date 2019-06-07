open Asmast
open Asmparser
open Asmlexer

let parse_asm = fun () ->
  MenhirLib.Convert.Simplified.traditional2revised Asmparser.section

let process channel =
  try
    (* Run the parser on this line of input. *)
    let lexd = Asmlexer.main channel in
    Printf.printf "%s\n%!" (Lexing.lexeme channel);
    Printf.printf "%s\n%!" (Asmast.show_section (Asmparser.section Asmlexer.main channel))
  with
  | Asmlexer.Error msg ->
    Printf.fprintf stderr "%s%!" msg
  | Asmparser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start channel)

let () =
 process (Lexing.from_channel stdin)
