open Location
open Common

type t = [%import: Location.t]
[@@deriving show]

type 'a located = [%import: 'a Location.located]
[@@deriving show]

type ident = string located [@@deriving show]
type hex   = B.zint located [@@deriving show]

type regimm = 
 | Reg of ident
 | Imm of B.zint located
[@@deriving show]

type operand =
  | Regimm  of regimm 
  | RegOffs of ident * regimm
  | Label   of ident * hex
[@@deriving show]

type operands = 
  | Ofixed    of operand list
  | Oflexible of ident list
[@@deriving show]

type stmt_r = {
  offset    : hex;
  instr_bin : hex;
  instr_asm : string;
  instr_exp : operands
} [@@deriving show]

type stmt = stmt_r located [@@deriving show]

type section_r = {
  s_adr    : hex;
  s_name   : ident;
  s_stmts  : stmt list
} [@@deriving show]

type section = section_r located [@@deriving show]
