type ident = string

type operand =
  | Reg of ident
  | RegShift of ident * string

type stmt = {
             offset    : string;
             instr_bin : string;
             instr_asm : string;
             instr_exp : operand list
             }

type section = {
                s_adr    : string;
                s_name   : ident;
                s_stmts  : stmt list
                }
