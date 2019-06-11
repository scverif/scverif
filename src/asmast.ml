open Location
open Common

type ident = string [@@deriving show]
type hex   = string [@@deriving show]


type operand =
  | Reg of ident
  | Imm of int
  | RegOffs of ident * operand
  | Label of ident list
[@@deriving show]

type stmt = {
             offset    : hex;
             instr_bin : hex;
             instr_asm : string;
             instr_exp : operand list
           } [@@deriving show]

type section = {
                s_adr    : hex;
                s_name   : ident;
                s_stmts  : stmt list
                } [@@deriving show]
