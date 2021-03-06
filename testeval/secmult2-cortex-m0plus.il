// Copyright 2019-2020 - Inria, NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "models/isa-cortex-m0plus.il"
include il "models/leakyisa-cortex-m0plus.il"
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

print:
  kind: evaltrace
  target: secmult_direct
  verbosity: 0;

infertaint:
  inputsAsPublic: true
  outputsAsPublic: true
  memoryAsPublicOut: true
  target: secmult_direct;

check:
  kind: SNI
  target: secmult_direct;
...
