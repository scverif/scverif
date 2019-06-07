type ident = string [@@deriving show]

type operand =
| Reg of ident
| RegShift of ident * string
[@@deriving show]

type stmt = {
             offset    : string;
             instr_bin : string;
             instr_asm : string;
             instr_exp : operand list
           } [@@deriving show]

type section = {
                s_adr    : string;
                s_name   : ident;
                s_stmts  : stmt list
                } [@@deriving show]
