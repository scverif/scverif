include il "isa/isa-cortex-m0plus.il"
include asm "testasm/secmult_flaw-cortex-m0plus-O3.objdump"

verbose 1

eval secmult_flaw
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 rnd[1:4]
  region mem w32 stack[-8:-1]
  init sp [stack 0]
  init r0 [c 0]
  init r1 [a 0]
  init r2 [b 0]
  init r3 [rnd 0]
  init lr exit;