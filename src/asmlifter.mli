val lift :
  Asmast.section -> Il.global list * (Il.macro * Ileval.initial) list

val lift_section :
  Asmast.section -> Ilast.command

val lift_stmt :
  Asmast.stmt -> Ilast.instr

val lift_operands :
  Asmast.operand list -> Ilast.macro_arg list
