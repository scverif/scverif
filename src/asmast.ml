type ident = string
type stmt = string

type section = {
                s_adr    : string;
                s_name   : ident;
                s_stmts  : stmt list
                }
