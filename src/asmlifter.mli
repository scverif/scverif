val lift_section :
  Asmast.section -> Ilast.command

val lift_stmt :
  string -> Asmast.stmt -> Ilast.cmd

val lift_operands :
  string -> Asmast.operand list -> Ilast.macro_arg list
