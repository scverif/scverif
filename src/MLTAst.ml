(* Copyright 2014-2020 - Inria *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)
(* -------------------------------------------------------------------- *)
open MLTStdLib

(* -------------------------------------------------------------------- *)
module Symbols : sig
  type symbol = private string

  val mksymbol     : string -> symbol
  val print_symbol : symbol -> string

  module Map : Map.S with type key = symbol
end = struct
  type symbol = string

  let mksymbol (x : string) : symbol     = x
  let print_symbol (x : symbol) : string = x

  module Map = Map.Make(String)

end

include Symbols

(* -------------------------------------------------------------------- *)
type location = {
  lc_fname : string;
  lc_start : int * int;
  lc_end   : int * int;
  lc_bchar : int;
  lc_echar : int;
}

type 'a located = { pl_data: 'a; pl_location: location; }

let mkloc (lc : location) (x : 'a) =
  { pl_data = x; pl_location = lc; }

let loc  { pl_location = lc } = lc
let data { pl_data     = dt } = dt

(* -------------------------------------------------------------------- *)
module Location : sig
  open Lexing

  type t = location

  val make      : position -> position -> t
  val of_lexbuf : lexbuf -> t
  val to_string : t -> string
end = struct
  open Lexing

  type t = location

  let make (p1 : position) (p2 : position) =
    let mkpos (p : position) = (p.pos_lnum, p.pos_cnum - p.pos_bol) in
    { lc_fname = p1.pos_fname;
      lc_start = mkpos p1    ;
      lc_end   = mkpos p2    ;
      lc_bchar = p1.pos_cnum ;
      lc_echar = p2.pos_cnum ; }

  let of_lexbuf (lexbuf : lexbuf) =
    let p1 = Lexing.lexeme_start_p lexbuf in
    let p2 = Lexing.lexeme_end_p lexbuf in
    make p1 p2

  let to_string (lc : t) =
    let spos =
      if lc.lc_start = lc.lc_end then
        Printf.sprintf "line %d (%d)"
          (fst lc.lc_start) (snd lc.lc_start)
      else if fst lc.lc_start = fst lc.lc_end then
        Printf.sprintf "line %d (%d-%d)"
          (fst lc.lc_start) (snd lc.lc_start) (snd lc.lc_end)
      else
        Printf.sprintf "line %d (%d) to line %d (%d)"
          (fst lc.lc_start) (snd lc.lc_start)
          (fst lc.lc_end  ) (snd lc.lc_end  )
    in
      Printf.sprintf "%s: %s" lc.lc_fname spos
end

(* -------------------------------------------------------------------- *)
exception ParseError of location * string option

(* -------------------------------------------------------------------- *)
type lsymbol = symbol located

(* -------------------------------------------------------------------- *)
type binop = 
 [`Eq | `Lt | `Gt | `Le | `Ge 
 | `Plus | `Minus | `Mul | `Div | `Mod | `Hat | `Amp | `Bar 
 | `LShift | `RShift]

type uniop = [`PostIncr | `PostDecr | `Tilde]

type asgop = [`Plus | `Minus | `Hat | `Amp | `Bar | `LShiftEq | `RShiftEq]

let binop_of_asgop = function
  | `Plus     -> `Plus
  | `Minus    -> `Minus
  | `Hat      -> `Hat
  | `Amp      -> `Amp
  | `Bar      -> `Bar
  | `LShiftEq -> `LShift
  | `RShiftEq -> `RShift

let binop_of_uniop = function
  | `PostIncr -> `Plus
  | `PostDecr -> `Minus

(* -------------------------------------------------------------------- *)
type lexpression_r = 
  | LEIdent of lsymbol 
  | LEArr   of lexpression * expression 

and lexpression = lexpression_r located

and expression_r =
| PEIdent  of lexpression
| PEAssign of lexpression * asgop option * expression
| PEInt    of Big_int_Z.big_int
| PEParens of expression
| PEUniOp  of uniop * expression
| PEBinOp  of binop * (expression * expression)
| PECall   of lsymbol * expression list
| PEArrDef of expression list

and expression = expression_r located

(* -------------------------------------------------------------------- *)
type int_size = [`W64 | `W32 | `W16 | `W8]
type int_sign = [`Unsigned | `Signed]
type int_t = int_size * int_sign 

let uint8_t  = `W8 , `Unsigned
let uint16_t = `W16, `Unsigned
let uint32_t = `W32, `Unsigned
let uint64_t = `W64, `Unsigned

let int8_t  = `W8 , `Signed
let int16_t = `W16, `Signed
let int32_t = `W32, `Signed
let int64_t = `W64, `Signed

(* FIXME arch dependent *)
let int_t  = int64_t


(* -------------------------------------------------------------------- *)

type sec_t = [ `Boolean of int_t ] (* boolean masking : xor *)

(* -------------------------------------------------------------------- *)
type type_r =
| TVoid 
| TInt of int_t
| TSec of sec_t  

and type_ = type_r located

(* -------------------------------------------------------------------- *)
let print_sign = function
  | `Unsigned -> "u" 
  | `Signed   -> ""

let print_size = function
  | `W8  -> "int8_t" 
  | `W16 -> "int16_t" 
  | `W32 -> "int32_t"
  | `W64 -> "int64_t"

let print_inttype (sz,sg) = 
  Format.sprintf "%s%s" (print_sign sg) (print_size sz) 

let print_sectype = function
  | `Boolean t -> "b" ^ (print_size (fst t))

let int_t_equal (t1:int_t) (t2:int_t) = t1 = t2

(* -------------------------------------------------------------------- *)
type instruction_r =
| PIExpression of expression
| PIFor        of forinstruction
| PIIf         of ifinstruction
| PIReturn     of expression option

and forinstruction = {
  pfi_init : expression option;
  pfi_test : expression option;
  pfi_post : expression option;
  pfi_body : statement;
}

and ifinstruction = {
  pii_test : expression;
  pii_then : statement;
  pii_else : statement option;
}

and instruction = instruction_r located
and statement   = instruction list

(* -------------------------------------------------------------------- *)
type lsymbol_decl = lsymbol * Big_int_Z.big_int located list

type localdecl_r = {
  ld_type : type_;
  ld_vars : (lsymbol_decl * expression option) list;
}

and localdecl = localdecl_r located

(* -------------------------------------------------------------------- *)
type pfunction_r = {
  fc_name  : lsymbol;
  fc_args  : (lsymbol_decl * type_) list;
  fc_retty : type_;
  fc_decls : localdecl list;
  fc_body  : instruction list;
}

and pfunction = pfunction_r located

(* -------------------------------------------------------------------- *)
type pglobal_r = 
  | PFun    of pfunction
  | PStatic of type_ * lsymbol_decl * expression 

type pglobal = pglobal_r located 

type pprogram = pglobal list
