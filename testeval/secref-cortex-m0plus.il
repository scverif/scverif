// Copyright 2019 - Inria, NXP

include il "isa/isa-cortex-m0plus.il"
include il "isa/leakyisa-cortex-m0plus.il"
include asm "testasm/secref-cortex-m0plus-O3.objdump"

annotate secrefresh
  region mem w32 a[0:2]
  region mem w32 rnd[0:4]
  region mem w32 stack[-2:0]
  init sp [stack 0]
  init r1 [a 0]
  init r3 [rnd 0]
  init lr exit
  input sharing a
  input urandom rnd
  output sharing a;

---
addleakage:
  target: secrefresh;

print:
  kind: macrodef
  target: secrefresh
  verbosity: 0;

inlinecall:
  target: secrefresh;

partialeval:
  target: secrefresh;

deadcodeelim:
  target: secrefresh;

print:
  kind: evaltrace
  target: secrefresh
  verbosity: 0;

check:
  kind: SNI
  target: secrefresh;
...
