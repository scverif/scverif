verbose 0
include il "isa/isa-cortex-m0plus.il"
verbose 1
include asm "testasm/secxor-cortex-m0plus-O3.objdump"
eval secxor
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 stack[0:2]
  init r1 [a 0]
  init r2 [b 0]
  init r0 [c 0]
  init sp [stack 12]
  init lr exit;
