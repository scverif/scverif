include il "isa/isa-cortex-m0plus.il"
include il "isa/leakyisa-cortex-m0plus.il"
include asm "testasm/secref-cortex-m0plus-O3.objdump"

verbose 0

apply addleakage
  secrefresh;

apply inline;

annotate secrefresh
  region mem w32 a[0:2]
  region mem w32 rnd[0:4]
  region mem w32 stack[-2:0]
  init sp [stack 0]
  init r1 [a 0]
  init r3 [rnd 0]
  init lr exit
  outcome a;

apply partialeval
  secrefresh;

apply deadcodeelim
  secrefresh;

print 0
  trace secrefresh;