// Copyright 2020 - Inria, NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "isa/isa-cortex-m0plus.il"

macro secxor ()
 label lbl1
{
  movs2 (r3, (w32) 0);
  push3 (r4, r5, lr);

lbl1: ldr3  (r4, r1, r3);
  ldr3  (r5, r2, r3);
  eors2 (r4, r5);
  str3  (r4, r0, r3);
  adds2 (r3, (w32) 4);
  if (r3 != ((w32) 12)) {
    goto lbl1;
  }

  pop3 (r4, r5, pc);
}

annotate secxor
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 stack[0:2]
  init r1 [a 0]
  init r2 [b 0]
  init r0 [c 0]
  init sp [stack 12]
  init lr exit
  input sharing a
  input sharing b
  input public stack
  output sharing c;

---
print:
  target: secxor
  kind: macrodef
  verbosity: 1;

addleakage:
  target: secxor;

inlinecall:
  target: secxor;

print:
  kind: macrodef
  target: secxor
  verbosity: 0;

partialeval:
  target: secxor;

print:
  kind: evaltrace
  target: secxor
  verbosity: 1;

rewriteformv:
  target: secxor
  inferinput: true
  inferoutput: true;

print:
  kind: maskverif
  target: secxor
  verbosity: 1;

check:
  target: secxor
  kind: NI;
...
