include il "isa/isa-cortex-m0plus.il"
include il "isa/leakyisa-cortex-m0plus.il"
include asm "testasm/secmult-cortex-m0plus-O3.objdump"

verbose 0

apply addleakage
  secmult;

apply inline;

annotate secmult
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
  init lr exit
  outcome c;

apply partialeval
  secmult;

apply deadcodeelim
  secmult;

print 0
  trace secmult;