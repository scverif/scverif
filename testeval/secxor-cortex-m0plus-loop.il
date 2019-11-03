// Copyright 2019 - Inria, NXP

include il "isa/isa-cortex-m0plus.il"
include il "isa/leakyisa-cortex-m0plus.il"
include asm "testasm/secxor-cortex-m0plus-Os.objdump"

annotate secxor
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 stack[-3:-1]
  init r0 [c 0]
  init r1 [a 0]
  init r2 [b 0]
  init sp [stack 0]
  init lr exit
  input sharing a
  input sharing b
  output sharing c;

---
addleakage:
  target: secxor;

print:
  kind: macrodef
  target: secxor
  verbosity: 0;

inlinecall:
  target: secxor;

partialeval:
  target: secxor;

deadcodeelim:
  target: secxor;

print:
  kind: evaltrace
  target: secxor
  verbosity: 0;

check:
  kind: NI
  target: secxor;
...
