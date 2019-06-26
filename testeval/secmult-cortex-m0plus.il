include il "isa/isa-cortex-m0plus.il"
include asm "testasm/secmult-cortex-m0plus-O3.objdump"

eval secmult
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 rnds[1:4]
  region mem w32 stack[-8:-1]
  init sp [stack 0]
  init r0 [c 0]
  init r1 [a 0]
  init r2 [b 0]
  init r3 [rnds 0] 
  init lr exit;
