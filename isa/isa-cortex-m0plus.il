// Copyright 2019 - NXP

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
  dst <- op;
}

macro movs2 (w32 dst, w32 op)
{
  mov2(dst, op);
  ExtractBit(dst, 31, apsrn);
  IsZero(op, apsrz);
}

macro adds3 (w32 dst, w32 op1, w32 op2)
{
  AddWithCarry(op1, op2, false, dst, apsrc, apsrv);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
}

macro adds2 (w32 op1, w32 op2)
{
  adds3(op1, op1, op2);
}

macro add3 (w32 dst, w32 op1, w32 op2)
  bool i, bool j
{
  AddWithCarry(op1, op2, false, dst, i, j);
}

macro add2 (w32 op1, w32 op2)
{
  add3(op1, op1, op2);
}

macro adcs2 (w32 op1, w32 op2)
{
  AddWithCarry(op1, op2, apsrc, op1, apsrc, apsrv);
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro sub3 (w32 dst, w32 op1, w32 op2)
  bool i, bool j
{
  SubWithCarry(op1, op2, dst, i, j);
}

macro sub2 (w32 op1, w32 op2)
{
  sub3(op1, op1, op2);
}

macro subs3 (w32 dst, w32 op1, w32 op2)
{
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
  add3(address, adr, ofs);
  dst <- [w32 mem (int) address];
}

macro ldrb3 (w32 dst, w32 adr, w32 ofs)
  w32 address
{
  add3(address, adr, ofs);
  dst <- zeroextend w32 [w8 mem (int) address];
}

macro ldrh3 (w32 dst, w32 adr, w32 ofs)
  w32 address
{
  add3(address, adr, ofs);
  dst <- zeroextend w32 [w16 mem (int) address];
}

macro str3 (w32 op, w32 adr, w32 ofs)
  w32 address
{
  add3(address, adr, ofs);
  [w32 mem (int) address] <- op;
}

macro strb3 (w32 op, w32 adr, w32 ofs)
  w32 address, w8 val
{
  add3(address, adr, ofs);
  val <- (w8) (uint) op;
  [w8 mem (int) address] <- val;
}

macro strh3 (w32 op, w32 adr, w32 ofs)
  w32 address, w16 val
{
  add3(address, adr, ofs);
  val <- (w16) (uint) op;
  [w16 mem (int) address] <- val;
}

macro sxtb2 (w32 dst, w32 src)
{
  dst <- signextend w32 (w8) ((uint) src);
}

macro sxth2 (w32 dst, w32 src)
{
  dst <- signextend w32 (w16) ((uint) src);
}

macro push1 (w32 op1)
{
  sub2(sp, (w32) 4);
  str3(op1, sp, (w32) 0);
}

macro push2 (w32 op1, w32 op2)
{
  sub2(sp, (w32) 8);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
}

macro push3 (w32 op1, w32 op2, w32 op3)
{
  sub2(sp, (w32) 12);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
}

macro push4 (w32 op1, w32 op2, w32 op3, w32 op4)
{
  sub2(sp, (w32) 16);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
}

macro push5 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5)
{
  sub2(sp, (w32) 20);
  str3(op1, sp, (w32) 0);
  str3(op2, sp, (w32) 4);
  str3(op3, sp, (w32) 8);
  str3(op4, sp, (w32) 12);
  str3(op5, sp, (w32) 16);
}

macro push6 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6)
{
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
  ldr3(op1, sp, (w32) 0);
  add2(sp, (w32) 4);
}

macro pop2 (w32 op1, w32 op2)
{
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  add2(sp, (w32) 8);
}

macro pop3 (w32 op1, w32 op2, w32 op3)
{
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  add2(sp, (w32) 12);
}

macro pop4 (w32 op1, w32 op2, w32 op3, w32 op4)
{
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  add2(sp, (w32) 16);
}

macro pop5 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5)
{
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  add2(sp, (w32) 20);
}

macro pop6 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6)
{
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  ldr3(op6, sp, (w32) 20);
  add2(sp, (w32) 24);
}

macro pop7 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7)
{
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  ldr3(op6, sp, (w32) 20);
  ldr3(op7, sp, (w32) 24);
  add2(sp, (w32) 28);
}

macro pop8 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7, w32 op8)
{
  ldr3(op1, sp, (w32) 0);
  ldr3(op2, sp, (w32) 4);
  ldr3(op3, sp, (w32) 8);
  ldr3(op4, sp, (w32) 12);
  ldr3(op5, sp, (w32) 16);
  ldr3(op6, sp, (w32) 20);
  ldr3(op7, sp, (w32) 24);
  ldr3(op8, sp, (w32) 28);
  add2(sp, (w32) 32);
}

macro pop9 (w32 op1, w32 op2, w32 op3, w32 op4, w32 op5, w32 op6, w32 op7, w32 op8, w32 op9)
{
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
}

macro ldmiaexcl2 (w32 adr, w32 dst1)
{
  ldr3(dst1, adr, (w32) 0);
  add2(adr, (w32) 4);
}

macro ldmia3 (bool wb, w32 adr, w32 dst1)
{
  ldr3(dst1, adr, (w32) 0);
  if wb {
    add2(adr, (w32) 4);
  }
}

macro lsls3 (w32 dst, w32 op1, w32 shift)
  int shift_n
{
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
  op1 <- op1 ^w32 op2;
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro ands2 (w32 op1, w32 op2)
{
  op1 <- op1 &w32 op2;
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro orrs2 (w32 op1, w32 op2)
{
  op1 <- op1 |w32 op2;
  ExtractBit(op1, 31, apsrn);
  IsZero(op1, apsrz);
}

macro rsbs2 (w32 op1, w32 op2)
{
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
  dst <- zeroextend w32 ((w8) ((int) src));
}

macro uxth2 (w32 dst, w32 src)
{
  dst <- zeroextend w32 ((w16) ((int) src));
}

macro cmp2 (w32 op1, w32 op2)
  w32 cmpr
{
  SubWithCarry(op1, op2, cmpr, apsrc, apsrv);
  ExtractBit(cmpr, 31, apsrn);
  IsZero(cmpr, apsrz);
}

macro mvns2 (w32 dst, w32 src)
  w32 ival
{
  LSL_C(src, 0, ival, apsrc);
  ExtractBit(dst, 31, apsrn);
  IsZero(dst, apsrz);
  dst <- !w32 ival;
}

/* Branching */
macro bn1 (label l)
{
  goto l;
}

macro beqn1 (label l)
{
  if (apsrz) { goto l; }
}

macro bnen1 (label l)
{
  if (!apsrz) { goto l; }
}

macro bcsn1 (label l)
{
  if (apsrc) { goto l; }
}

macro bccn1 (label l)
{
  if (!apsrc) { goto l; }
}

macro bmin1 (label l)
{
  if (apsrn) { goto l; }
}

macro bpln1 (label l)
{
  if (!apsrn) { goto l; }
}

macro bvsn1 (label l)
{
  if (apsrv) { goto l; }
}

macro bvcn1 (label l)
{
  if (!apsrv) { goto l; }
}

macro bhin1 (label l)
{
  if (apsrc & !apsrz)
  { goto l; }
}

macro blsn1 (label l)
{
  if (!apsrc | apsrz)
  { goto l; }
}

macro bgen1 (label l)
{
  if (apsrn == apsrv)
  { goto l; }
}

macro bltn1 (label l)
{
  if (apsrn != apsrv)
  { goto l; }
}

macro bgtn1 (label l)
{
  if (!apsrz & (apsrn == apsrv))
  { goto l; }
}

macro blen1 (label l)
{
  if (apsrz | (apsrn != apsrv))
  { goto l; }
}

macro bl1 (label l)
{
  lr <- pc +w32 (w32) 4;
  goto l;
}

macro bx1 (w32 adr)
{
  pc <- adr;
}




macro nop0 ()
{}

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