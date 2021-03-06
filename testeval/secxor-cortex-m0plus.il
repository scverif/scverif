// Copyright 2019-2020 - Inria, NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "models/isa-cortex-m0plus.il"
include il "models/leakyisa-cortex-m0plus.il"
include asm "testasm/secxor-cortex-m0plus-O3.objdump"

annotate secxor
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 stack[-2:-1]
  init r1 [a 0]
  init r2 [b 0]
  init r0 [c 0]
  init sp [stack 0]
  init lr exit
  input sharing a
  input sharing b
  output sharing c
  /* everything else assumed to be public input !and! public output */;

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

infertaint:
  inputsAsPublic: true
  outputsAsPublic: true
  memoryAsPublicOut: true
  target: secxor;

check:
  kind: NI
  target: secxor;
...
