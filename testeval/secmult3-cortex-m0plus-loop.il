// Copyright 2019-2020 - Inria, NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "models/isa-cortex-m0plus.il"
include il "models/leakyisa-cortex-m0plus.il"
include asm "testasm/secmult_flaw-cortex-m0plus-Os.objdump"

annotate secmult_flaw
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 rnd[1:4]
  region mem w32 stack[-10:-1]
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
  target: secmult_flaw;

print:
  kind: macrodef
  target: secmult_flaw
  verbosity: 0;

inlinecall:
  target: secmult_flaw;

partialeval:
  target: secmult_flaw;

deadcodeelim:
  target: secmult_flaw;

print:
  kind: evaltrace
  target: secmult_flaw
  verbosity: 0;

infertaint:
  inputsAsPublic: true
  outputsAsPublic: true
  memoryAsPublicOut: true
  target: secmult_flaw;

check:
  kind: SNI
  target: secmult_flaw;
...
