(* Copyright 2020 - NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

%{
  open Location
  open Utils
  open Ilast
%}
%token EOF COMMA COLON SHARP EXCLAMATION PLUS
%token DALIGN DDATA DBSS DCPU DGLOBAL DSYNTAX DTEXT DTHUMB DTHUMBFUNC DTYPE
%token LBRACKET RBRACKET LCURLY RCURLY
%token <string> IDENT COMMENT
%token <Bigint.zint> INT

%start gasfile
%type <(Ilast.macro_decl Location.located) list> gasfile

%%

%inline loc(X):
  | x=X
    { { pl_desc = x; pl_loc = Location.make $startpos $endpos; } }

ident:
  | x=loc(IDENT) { x }

(* TODO some are quite dangerous and should lead to a big fat warning
   GAS input is anyway not considered to be representative for security due to ambiguity *)
gasignores:
  | DALIGN INT? COMMENT* {}
  | DBSS COMMENT* {}
  | DCPU ident COMMENT* {}
  | DDATA COMMENT* {}
  | DGLOBAL ident COMMENT* {}
  | DSYNTAX ident COMMENT* {}
  | DTEXT COMMENT* {}
  | DTHUMB COMMENT* {}
  | DTHUMBFUNC COMMENT* {}

regident:
  | r=ident { Aexpr (mk_loc (loc r) (Evar { r with pl_desc = unloc r } )) }

%inline operand:
  | r=regident
    { [r] }
  | SHARP i=loc(INT)
    { [Aexpr (mk_cast_w U32 (mk_int i))] }
  | LBRACKET r=regident l=loc(RBRACKET)
    { [r; Aexpr (mk_cast_w U32 (mk_loc (loc l) (Eint  Common.B.zero)))] }
  | LBRACKET r=regident COMMA SHARP i=loc(INT) RBRACKET
    { [r; Aexpr (mk_cast_w U32 (mk_int i))] }

operands:
  | LCURLY regs=separated_list(COMMA, regident) RCURLY
    { regs }
  | ops=separated_nonempty_list(COMMA, operand)
    { List.flatten ops }
  | ra=regident l=loc(EXCLAMATION) COMMA LCURLY regs=separated_list(COMMA, regident) RCURLY
    { ra::(Aexpr (mk_loc (loc l) (Ebool true)))::regs }
  | ra=regident                    COMMA LCURLY regs=separated_list(COMMA, regident) RCURLY
    { ra::(Aexpr (mk_loc (Location.make $startpos $endpos) (Ebool false)))::regs }

gaslabel:
  | l=ident
    { {l_base = unloc l; l_offs = Common.B.zero; l_loc = loc l} }
  | l=ident PLUS ofs=INT
    { {l_base = unloc l; l_offs = ofs; l_loc = loc l} }

gasstmt:
  | m=ident args=operands COMMENT*
    { Imacro(m, args) }
  | l=gaslabel COLON COMMENT*
    { Ilabel l }
  | error
    { parse_error (Location.make $startpos $endpos) "gasparser: invalid statement" }


%inline gasmacro:
  | gasignores* DTYPE gname=ident ident COMMENT* name=ident COLON COMMENT* cs=nonempty_list(loc(gasstmt))
    { if (String.equal (unloc gname) (unloc name)) then
        begin
          (* generate labels for each instruction *)
          let mk_lbl ofs l = {pl_desc=Ilabel({l_base=unloc name; l_offs=Common.B.of_int (ofs*2);
                                            l_loc=l});
                            pl_loc=loc name} in
          let cs' = (mk_lbl 0 (loc name))::
                      List.fold_righti
                        (fun ofs i is ->
                        (* the offset size depends on the instruction
                        encoding, thus constant +2 or +4 is unsound *)
                        (*(mk_lbl ofs (loc i))::*)(
                         match i with
                         (* concatenate number of arguments to called macros *)
                         | {pl_desc=Imacro(m, args); pl_loc=l} ->
                            {pl_desc =
                               Imacro(mk_loc (loc m)
                                             (String.lowercase (unloc m) ^
                                                (string_of_int (List.length args))),
                                      args);
                             pl_loc=l}
                        | _ -> i)::is)
                      cs [] in
          (* labels must be declared as local variables *)
          let locals = List.fold_left (fun ls i ->
                                        match unloc i with
                                        | Ilabel l -> Plabel l :: ls
                                        | _ -> ls) [] cs' in
          mk_loc (Location.make $startpos $endpos)
                 { mc_name = name; mc_params = []; mc_locals = locals; mc_body = cs' }
        end
      else
        parse_error (Location.make $startpos $endpos)
                    "gasparser: global name and label do not match" }

gasfile:
  | COMMENT* ms=gasmacro+ EOF
    { ms }
  | error
    { parse_error (Location.make $startpos $endpos) "gasparser: invalid gasfile" }
