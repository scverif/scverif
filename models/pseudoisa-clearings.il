// Copyright 2020 - NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

include il "models/pseudoisa-metrics.il"

int metric_scrubcount;
int metric_clearcount_opA;
int metric_clearcount_opB;
int metric_clearcount_opR;
int metric_clearcount_opW;

// clear regular state
// asm: movs(r, r0)
macro scrub(w32 r) {
  r <- pub;
  metric_scrubcount <- metric_scrubcount + 1;
  metric_cyclecount <- metric_cyclecount + 1;
}

macro clear(w32 l) {
  if l =name= opA {
    // asm: ands(r0, r0)
    opA <- pub;
    metric_clearcount_opA <- metric_clearcount_opA + 1;
    metric_cyclecount <- metric_cyclecount + 1;
  }
  if l =name= opB {
    // asm: ands(r0, r0)
    opB <- pub;
    metric_clearcount_opB <- metric_clearcount_opB + 1;
    metric_cyclecount <- metric_cyclecount + 1;
  }
  if l =name= opR {
    // asm: load(r0, r0, 0)
    opR <- pub;
    metric_clearcount_opR <- metric_clearcount_opR + 1;
    metric_cyclecount <- metric_cyclecount + 2;
  }
  if l =name= opW {
    // asm: store(r0, r0, 0)
    opW <- pub;
    metric_clearcount_opW <- metric_clearcount_opW + 1;
    metric_cyclecount <- metric_cyclecount + 2;
  }
}

/* deprecated */

// asm: ands(r0, r0)
macro clear_opA ()
{
  clear(opA);
}

// asm: ands(r0, r0)
macro clear_opB ()
{
  clear(opB);
}

// asm: load(r0, r0, 0)
macro clear_opR ()
{
  clear(opR);
}

// asm: store(r0, r0, 0)
macro clear_opW ()
{
  clear(opW);
}

macro clear_all ()
{
  clear(opA);
  clear(opB);
  clear(opR);
  clear(opW);
}

macro load_pub(w32 r) {
  scrub(r);
  clear(opR);
  clear(opB);
}