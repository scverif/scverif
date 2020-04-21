// Copyright 2019-2020 - NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "models/pseudoisa-clearings.il"

macro secxor ()
{
  load(r4, r1, 0);
  load(r5, r2, 0);
  xor(r4, r5);
  store(r4, r0, 0);

clear_all();

  load(r4, r1, 1);
  load(r5, r2, 1);
  xor(r5, r4);
  store(r4, r0, 1);

clear_all();

  load(r4, r1, 2);
  load(r5, r2, 2);
  xor(r4, r5);
  store(r4, r0, 2);
}

annotate secxor
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 rnd[0:9]
  init r0 [c 0]
  init r1 [a 0]
  init r2 [b 0]
  init r3 [rnd 0]
  input sharing a
  input sharing b
  input urandom rnd
  output sharing c
  init metric_cyclecount 0
  init metric_scrubcount 0
  init metric_clearcount_opA 0
  init metric_clearcount_opB 0
  init metric_clearcount_opR 0
  init metric_clearcount_opW 0
  init metric_ops_xor 0
  init metric_ops_and 0
  init metric_ops_copy 0
  init metric_ops_load 0
  init metric_ops_store 0
  ;

---
verbosity:
  verbosity: 1;

print:
  kind: macrodef
  target: load
  verbosity: 0;

/* put all leakages of a macro in a single leak statement at the beginning.
accumulate:
  target: [load,store,xor,and]
  leaks: any
  keep: false; // remove the old leakages
*/

print:
  kind: macrodef
  target: load
  verbosity: 0;

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

infertaint:
  inputsAsPublic: true
  outputsAsPublic: true
  memoryAsPublicOut: true
  target: secxor;

print:
  kind: maskverif
  target: secxor
  verbosity: 0;

print:
  kind: variables
  variables: "metric_*"
  target: secxor
  verbosity: 1;

check:
  target: secxor
  kind: NI;
...
