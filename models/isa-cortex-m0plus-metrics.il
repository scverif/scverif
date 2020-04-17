// Copyright 2019-2020 - NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

w32 r0;
w32 r1;
w32 r2;
w32 r3;
w32 r4;
w32 r5;
w32 r6;
w32 r7;
w32 r8;
w32 r9;
w32 sl;
w32 fp;
w32 ip;
w32 sp;
w32 lr;
w32 pc;

bool apsrn;
bool apsrz;
bool apsrc;
bool apsrv;

int metric_cyclecount;
int metric_ops_xor;
int metric_ops_and;
int metric_ops_copy;
int metric_ops_load;
int metric_ops_store;

mem [];

/************** HELPERS **************/
macro AddWithCarry (w32 x, w32 y, bool carry_in, w32 result, bool carry_out, bool overflow)
  int unsigned_sum, int signed_sum
{
  unsigned_sum <- (uint) x + (uint) y + (uint) carry_in;
  signed_sum   <- (int) x  + (int) y  + (uint) carry_in;
  result       <- (w32) unsigned_sum;
  carry_out    <- ! ((uint) result == unsigned_sum);
  overflow     <- ! ((int)  result ==   signed_sum);
}

macro SubWithCarry (w32 x, w32 y, w32 result, bool carry_out, bool overflow)
  int unsigned_sub, int signed_sub
{
  unsigned_sub <- (uint) x - (uint) y;
  signed_sub   <- (int) x  - (int) y + 1;
  result       <- (w32) unsigned_sub;
  carry_out    <- ! ((uint) result == unsigned_sub);
  overflow     <- ! ((int)  result ==   signed_sub);
}

macro IsZero (w32 x, bool result)
{
  result <- (uint) x == 0;
}

macro ExtractBit (w32 x, int i, bool result)
/* counting from zero */
{
  result <- (x >>w32 i) ==w32 (w32) 1;
}

macro LSL_C (w32 x, int shift, w32 result, bool carry_out)
{
  result <- x <<w32 (uint) shift;
  ExtractBit(x, (31 - (uint) shift), carry_out);
}

macro LSR_C (w32 x, int shift, w32 result, bool carry_out)
{
  result <- x >>uw32 (uint) shift;
  ExtractBit(x, ((uint) shift - 1), carry_out);
}

macro ASR_C (w32 x, int shift, w32 result, bool carry_out)
{
  result <- x >>sw32 (uint) shift;
  ExtractBit(x, ((uint) shift - 1), carry_out);
}

/************** INSTRUCTIONS **************/

macro mov2 (w32 dst, w32 op)
{
  metric_ops_copy <- metric_ops_copy + 1;
  dst <- op;
  if (dst =name= pc) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto dst;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro movs2 (w32 dst, w32 op)
{
  ExtractBit(op, 31, apsrn);
  IsZero(op, apsrz);
  mov2(dst, op);
}

macro adds3 (w32 dst, w32 op1, w32 op2)
{
  AddWithCarry(op1, op2, false, dst, apsrc, apsrv);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
  if (dst =name= pc) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto dst;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro adds2 (w32 op1, w32 op2)
{
  adds3(op1, op1, op2);
}

macro add3 (w32 dst, w32 op1, w32 op2)
  bool i, bool j
{
  AddWithCarry(op1, op2, false, dst, i, j);
  if (dst =name= pc) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto dst;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro add2 (w32 op1, w32 op2)
{
  add3(op1, op1, op2);
}

macro adcs2 (w32 dst, w32 op2)
{
  metric_cyclecount <- metric_cyclecount + 1;
  AddWithCarry(dst, op2, apsrc, dst, apsrc, apsrv);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
}

macro sub3 (w32 dst, w32 op1, w32 op2)
  bool i, bool j
{
  metric_cyclecount <- metric_cyclecount + 1;
  SubWithCarry(op1, op2, dst, i, j);
}

macro sub2 (w32 op1, w32 op2)
{
  sub3(op1, op1, op2);
}

macro subs3 (w32 dst, w32 op1, w32 op2)
{
  metric_cyclecount <- metric_cyclecount + 1;
  SubWithCarry(op1, op2, dst, apsrc, apsrv);
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro subs2 (w32 op1, w32 op2)
{
  subs3(op1, op1, op2);
}

macro ldr3 (w32 dst, w32 adr, w32 ofs)
  w32 address
{
  metric_cyclecount <- metric_cyclecount + 2;
  metric_ops_load <- metric_ops_load + 1;
  add3(address, adr, ofs);
  dst <- [w32 mem (int) address];
}

macro ldrb3 (w32 dst, w32 adr, w32 ofs)
  w32 address
{
  metric_cyclecount <- metric_cyclecount + 1;
  metric_ops_load <- metric_ops_load + 1;
  add3(address, adr, ofs);
  dst <- zeroextend w32 [w8 mem (int) address];
}

macro ldrh3 (w32 dst, w32 adr, w32 ofs)
  w32 address
{
  metric_cyclecount <- metric_cyclecount + 1;
  metric_ops_load <- metric_ops_load + 1;
  add3(address, adr, ofs);
  dst <- zeroextend w32 [w16 mem (int) address];
}

macro str3 (w32 op, w32 adr, w32 ofs)
  w32 address
{
  metric_cyclecount <- metric_cyclecount + 1;
  metric_ops_store <- metric_ops_store + 1;
  add3(address, adr, ofs);
  [w32 mem (int) address] <- op;
}

macro strb3 (w32 op, w32 adr, w32 ofs)
  w32 address, w8 val
{
  metric_cyclecount <- metric_cyclecount + 1;
  metric_ops_store <- metric_ops_store + 1;
  add3(address, adr, ofs);
  val <- (w8) (uint) op;
  [w8 mem (int) address] <- val;
}

macro strh3 (w32 op, w32 adr, w32 ofs)
  w32 address, w16 val
{
  metric_cyclecount <- metric_cyclecount + 1;
  metric_ops_store <- metric_ops_store + 1;
  add3(address, adr, ofs);
  val <- (w16) (uint) op;
  [w16 mem (int) address] <- val;
}

macro sxtb2 (w32 dst, w32 src)
{
  metric_cyclecount <- metric_cyclecount + 1;
  dst <- signextend w32 (w8) ((uint) src);
}

macro sxth2 (w32 dst, w32 src)
{
  metric_cyclecount <- metric_cyclecount + 1;
  dst <- signextend w32 (w16) ((uint) src);
}

macro stmia4 (w32 adr, bool increment, w32 val1, w32 val2)
{
  metric_cyclecount <- metric_cyclecount - 1;
  metric_ops_store <- metric_ops_store + 2;
  str3(val1, adr, (w32) 0);
  str3(val2, adr, (w32) 4);
  if (increment) {
    metric_cyclecount <- metric_cyclecount - 1;
    add2(adr, (w32) 8);
  }
}

macro stmia5 (w32 adr, bool increment, w32 val1, w32 val2, w32 val3)
{
  metric_cyclecount <- metric_cyclecount - 2;
  metric_ops_store <- metric_ops_store + 3;
  str3(val1, adr, (w32) 0);
  str3(val2, adr, (w32) 4);
  str3(val3, adr, (w32) 8);
  if (increment) {
    metric_cyclecount <- metric_cyclecount - 1;
    add2(adr, (w32) 12);
  }
}

macro stmia6 (w32 adr, bool increment, w32 val1, w32 val2, w32 val3, w32 val4)
{
  metric_cyclecount <- metric_cyclecount - 3;
  metric_ops_store <- metric_ops_store + 4;
  str3(val1, adr, (w32) 0);
  str3(val2, adr, (w32) 4);
  str3(val3, adr, (w32) 8);
  str3(val4, adr, (w32) 12);
  if (increment) {
    metric_cyclecount <- metric_cyclecount - 1;
    add2(adr, (w32) 16);
  }
}

macro push1 (w32 op1)
{
  metric_cyclecount <- metric_cyclecount - 1;
  metric_ops_store <- metric_ops_store + 1;
  sub2(sp, (w32) 4);
  str3(op1, sp, (w32) 0);
}

macro push2 (w32 op1, w32 op2)
{
  metric_cyclecount <- metric_cyclecount - 2;
  metric_ops_store <- metric_ops_store + 2;
  sub2(sp, (w32) 8);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
}

macro push3 (w32 op1, w32 op2, w32 op3)
{
  metric_cyclecount <- metric_cyclecount - 3;
  metric_ops_store <- metric_ops_store + 3;
  sub2(sp, (w32) 12);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
}

macro push4 (w32 op1, w32 op2, w32 op3, w32 op4)
{
  metric_cyclecount <- metric_cyclecount - 4;
  metric_ops_store <- metric_ops_store + 4;
  sub2(sp, (w32) 16);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
}

macro push5 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5)
{
  metric_cyclecount <- metric_cyclecount - 5;
  metric_ops_store <- metric_ops_store + 5;
  sub2(sp, (w32) 20);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
  str3(op5, sp, (w32) 16);
}

macro push6 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6)
{
  metric_cyclecount <- metric_cyclecount - 6;
  metric_ops_store <- metric_ops_store + 6;
  sub2(sp, (w32) 24);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
  str3(op5, sp, (w32) 16);
  str3(op6, sp, (w32) 20);
}

macro push7 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7)
{
  metric_cyclecount <- metric_cyclecount - 7;
  metric_ops_store <- metric_ops_store + 7;
  sub2(sp, (w32) 28);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
  str3(op5, sp, (w32) 16);
  str3(op6, sp, (w32) 20);
  str3(op7, sp, (w32) 24);
}

macro push8 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7, w32 op8)
{
  metric_cyclecount <- metric_cyclecount - 8;
  metric_ops_store <- metric_ops_store + 8;
  sub2(sp, (w32) 32);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
  str3(op5, sp, (w32) 16);
  str3(op6, sp, (w32) 20);
  str3(op7, sp, (w32) 24);
  str3(op8, sp, (w32) 28);
}

macro push9 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7, w32 op8, w32 op9)
{
  metric_cyclecount <- metric_cyclecount - 9;
  metric_ops_store <- metric_ops_store + 9;
  sub2(sp, (w32) 36);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
  str3(op5, sp, (w32) 16);
  str3(op6, sp, (w32) 20);
  str3(op7, sp, (w32) 24);
  str3(op8, sp, (w32) 28);
  str3(op9, sp, (w32) 32);
}

macro pop1 (w32 op1)
{
  metric_ops_load <- metric_ops_load + 1;
  ldr3(op1, sp, (w32) 0);
  add2(sp, (w32) 4);
  if (op1 =name= pc) {
    metric_cyclecount <- metric_cyclecount + 1;
    goto op1;
  } else {
    metric_cyclecount <- metric_cyclecount - 1;
  }
}

macro pop2 (w32 op1, w32 op2)
{
  metric_ops_load <- metric_ops_load + 2;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  add2(sp, (w32) 8);
  if (op2 =name= pc) {
    metric_cyclecount <- metric_cyclecount;
    goto op2;
  } else {
    metric_cyclecount <- metric_cyclecount - 2;
  }
}

macro pop3 (w32 op1, w32 op2, w32 op3)
{
  metric_ops_load <- metric_ops_load + 3;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  add2(sp, (w32) 12);
  if (op3 =name= pc) {
    metric_cyclecount <- metric_cyclecount - 1;
    goto op3;
  } else {
    metric_cyclecount <- metric_cyclecount - 3;
  }
}

macro pop4 (w32 op1, w32 op2, w32 op3, w32 op4)
{
  metric_ops_load <- metric_ops_load + 4;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  add2(sp, (w32) 16);
  if (op4 =name= pc) {
    metric_cyclecount <- metric_cyclecount - 2;
    goto op4;
  } else {
    metric_cyclecount <- metric_cyclecount - 4;
  }
}

macro pop5 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5)
{
  metric_ops_load <- metric_ops_load + 5;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  add2(sp, (w32) 20);
  if (op5 =name= pc) {
    metric_cyclecount <- metric_cyclecount - 3;
    goto op5;
  } else {
    metric_cyclecount <- metric_cyclecount - 5;
  }
}

macro pop6 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6)
{
  metric_ops_load <- metric_ops_load + 6;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  ldr3(op6, sp, (w32) 20);
  add2(sp, (w32) 24);
  if (op6 =name= pc) {
    metric_cyclecount <- metric_cyclecount - 4;
    goto op6;
  } else {
    metric_cyclecount <- metric_cyclecount - 6;
  }
}

macro pop7 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7)
{
  metric_ops_load <- metric_ops_load + 7;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  ldr3(op6, sp, (w32) 20);
  ldr3(op7, sp, (w32) 24);
  add2(sp, (w32) 28);
  if (op7 =name= pc) {
    metric_cyclecount <- metric_cyclecount - 5;
    goto op7;
  } else {
    metric_cyclecount <- metric_cyclecount - 7;
  }
}

macro pop8 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7, w32 op8)
{
  metric_ops_load <- metric_ops_load + 8;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  ldr3(op6, sp, (w32) 20);
  ldr3(op7, sp, (w32) 24);
  ldr3(op8, sp, (w32) 28);
  add2(sp, (w32) 32);
  if (op8 =name= pc) {
    metric_cyclecount <- metric_cyclecount - 6;
    goto op8;
  } else {
    metric_cyclecount <- metric_cyclecount - 8;
  }
}

macro pop9 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7, w32 op8, w32 op9)
{
  metric_ops_load <- metric_ops_load + 7;
  metric_cyclecount <- metric_cyclecount - 7;
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  ldr3(op6, sp, (w32) 20);
  ldr3(op7, sp, (w32) 24);
  ldr3(op8, sp, (w32) 28);
  ldr3(op9, sp, (w32) 32);
  add2(sp, (w32) 36);
  goto op9; // op9 must be pc, always jumping
}

macro ldmia3 (w32 adr, bool wb, w32 dst1)
  w32 address
{
  metric_ops_load <- metric_ops_load + 1;
  metric_cyclecount <- metric_cyclecount - 1;
  address <- adr;
  if wb {
    add2(adr, (w32) 4);
  }
  ldr3(dst1, address, (w32) 0);
}

macro lsls3 (w32 dst, w32 op1, w32 shift)
  int shift_n
{
  metric_cyclecount <- metric_cyclecount + 1;
  shift_n <- (uint) shift;
  LSL_C(op1, shift_n, dst, apsrc);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
}

macro lsls2 (w32 op1, w32 op2)
{
  lsls3(op1, op1, op2);
}

macro lsrs3 (w32 dst, w32 op1, w32 shift)
  int shift_n
{
  metric_cyclecount <- metric_cyclecount + 1;
  shift_n <- (uint) shift;
  LSR_C(op1, shift_n, dst, apsrc);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
}

macro lsrs2 (w32 op1, w32 op2)
{
  lsrs3(op1, op1, op2);
}

macro asrs3 (w32 dst, w32 op1, w32 shift)
  int shift_n
{
  metric_cyclecount <- metric_cyclecount + 1;
  shift_n <- (uint) shift;
  ASR_C(op1, shift_n, dst, apsrc);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
}

macro asrs2 (w32 dst, w32 op1)
{
  asrs3(dst, op1, op1);
}

macro eors2 (w32 op1, w32 op2)
{
  metric_cyclecount <- metric_cyclecount + 1;
  metric_ops_xor <- metric_ops_xor + 1;
  op1 <- op1 ^w32 op2;
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro ands2 (w32 op1, w32 op2)
{
  metric_cyclecount <- metric_cyclecount + 1;
  metric_ops_and <- metric_ops_and + 1;
  op1 <- op1 &w32 op2;
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro orrs2 (w32 op1, w32 op2)
{
  metric_cyclecount <- metric_cyclecount + 1;
  op1 <- op1 |w32 op2;
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro rsbs2 (w32 op1, w32 op2)
{
  metric_cyclecount <- metric_cyclecount + 1;
  AddWithCarry(! w32 op1, op2, true, op1, apsrc, apsrv);
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro negs2 (w32 op1, w32 op2)
{
  rsbs2(op1, op2);
}

macro uxtb2 (w32 dst, w32 src)
{
  metric_cyclecount <- metric_cyclecount + 1;
  dst <- zeroextend w32 ((w8) ((int) src));
}

macro uxth2 (w32 dst, w32 src)
{
  metric_cyclecount <- metric_cyclecount + 1;
  dst <- zeroextend w32 ((w16) ((int) src));
}

macro cmp2 (w32 op1, w32 op2)
  w32 cmpr
{
  metric_cyclecount <- metric_cyclecount + 1;
  AddWithCarry(op1, !w32 op2, true, cmpr, apsrc, apsrv);
  ExtractBit(cmpr, 31, apsrn);
  IsZero(cmpr, apsrz);
}

macro mvns2 (w32 dst, w32 src)
  w32 ival
{
  metric_cyclecount <- metric_cyclecount + 1;
  LSL_C(src, 0, ival, apsrc);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
  dst <- !w32 ival;
}

/* Branching */
macro bn1 (label l)
{
  metric_cyclecount <- metric_cyclecount + 2;
  goto l;
}

macro beqn1 (label l)
{
  if (apsrz) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bnen1 (label l)
{
  if (!apsrz) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bcsn1 (label l)
{
  if (apsrc) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bccn1 (label l)
{
  if (!apsrc) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bmin1 (label l)
{
  if (apsrn) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bpln1 (label l)
{
  if (!apsrn) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bvsn1 (label l)
{
  if (apsrv) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bvcn1 (label l)
{
  if (!apsrv) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bhin1 (label l)
{
  if (apsrc & !apsrz) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro blsn1 (label l)
{
  if (!apsrc | apsrz) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bgen1 (label l)
{
  if (apsrn == apsrv) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bltn1 (label l)
{
  if (apsrn != apsrv) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bgtn1 (label l)
{
  if (!apsrz & (apsrn == apsrv)) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro blen1 (label l)
{
  if (apsrz | (apsrn != apsrv)) {
    metric_cyclecount <- metric_cyclecount + 2;
    goto l;
  } else {
    metric_cyclecount <- metric_cyclecount + 1;
  }
}

macro bl1 (label l)
{
  metric_cyclecount <- metric_cyclecount + 3;
  lr <- pc +w32 (w32) 4;
  goto l;
}

macro bx1 (w32 adr)
{
  metric_cyclecount <- metric_cyclecount + 2;
  pc <- adr;
}




macro nop0 ()
{
  metric_cyclecount <- metric_cyclecount + 1;
}

// FIXME: asmlifter needs to take care of it
macro word1 (w32 i)
{}

// TODO: make usefull
macro rand ()
  label rand
{
  rand:
  r0 <- r0 +w32 (w32) 0;
}

macro memset ()
  label memset
{

}

macro memcpy ()
  label memcpy
{

}
