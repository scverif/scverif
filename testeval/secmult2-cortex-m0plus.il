// Copyright 2019 - Inria, NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "isa/isa-cortex-m0plus.il"
include il "isa/leakyisa-cortex-m0plus.il"
include asm "testasm/secmult_direct-cortex-m0plus-O3.objdump"

annotate secmult_direct
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 rnd[1:4]
  region mem w32 stack[-22:-1]
  init sp [stack 0]
  init r0 [c 0]
  init r1 [a 0]
  init r2 [b 0]
  init r3 [rnd 0]
  init lr exit
  input sharing a
  input sharing b
  input urandom rnd
  output sharing c;

---
addleakage:
  target: secmult_direct;

print:
  kind: macrodef
  target: secmult_direct
  verbosity: 0;

inlinecall:
  target: secmult_direct;

partialeval:
  target: secmult_direct;

deadcodeelim:
  target: secmult_direct;

print:
  kind: evaltrace
  target: secmult_direct
  verbosity: 0;

check:
  kind: SNI
  target: secmult_direct;
...
