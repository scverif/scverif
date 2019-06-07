open Asmast
open Asmparser
open Asmlexer

let process channel =
  try
    (* Run the parser on this line of input. *)
    Printf.printf "%s\n%!" (Asmast.show_section (Asmparser.section Asmlexer.main channel))
  with
  | Asmlexer.Error msg ->
    Printf.fprintf stderr "%s%!" msg
  | Asmparser.Error ->
    Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start channel)

let () =
 process (Lexing.from_channel stdin)
