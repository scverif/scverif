%{
  open Location
  open Utils
  open Mainast
  let parse_error loc msg = raise (ParseError (loc, msg))
%}

%token READ ASM IL EOF
%token <string>STRING

%start command
%start file

%type <Mainast.command>command
%type <Mainast.command list> file

%%

%inline loc(X):
| x=X { { pl_desc = x; pl_loc = Location.make $startpos $endpos; } }
;

command1:
  | READ ASM s=loc(STRING) { Read (Asm, s) }
  | READ IL  s=loc(STRING) { Read (Il, s) }
  | error             { parse_error (Location.make $startpos $endpos) None }

command:
  | c=command1 { c }
  | EOF        { Exit }

file:
  | c=command1* EOF { c }
