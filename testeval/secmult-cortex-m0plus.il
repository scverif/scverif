// Copyright 2019-2020 - Inria, NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "models/isa-cortex-m0plus.il"
include il "models/leakyisa-cortex-m0plus.il"
include asm "testasm/secmult-cortex-m0plus-O3.objdump"

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
  input sharing a
  input sharing b
  input urandom rnd
  output sharing c;

---
addleakage:
  target: [ldr3, secmult];

print:
  kind: macrodef
  target: secmult
  verbosity: 0;

inlinecall:
  target: secmult;

partialeval:
  target: secmult;

print:
  kind: evaltrace
  target: secmult
  verbosity: 0;

infertaint:
  inputsAsPublic: true
  outputsAsPublic: true
  memoryAsPublicOut: true
  target: secmult;

check:
  kind: SNI
  target: secmult;
...
