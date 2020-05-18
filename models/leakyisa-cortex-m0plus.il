// Copyright 2019 - NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

w32 opA;
w32 opB;
w32 opR;
w32 opW;

macro eors2_leak (w32 op1, w32 op2)
{
  leak eorsCompResult (op1 ^w32 op2);
  leak eorsTransition (op1, op1 ^w32 op2);
  leak eorsOperand (opA, op1, opB, op2);
  leak eorsOperandA (opA, op1);
  leak eorsOperandB (opB, op2);

  opA <- op1;
  opB <- op2;
}

macro ands2_leak (w32 op1, w32 op2)
{
  leak andsCompResult (op1 &w32 op2);
  leak andsTransition (op1, op1 &w32 op2);
  leak andsOperand (opA, op1, opB, op2);
  leak andsOperandA (opA, op1);
  leak andsOperandB (opB, op2);

  opA <- op1;
  opB <- op2;
}

macro ldr3_leak (w32 dst, w32 adr, w32 ofs)
  w32 val
{
  val <- [w32 mem (int) (adr +w32 ofs)];

  leak ldrOperand1 (opA, adr, opB, ofs);

  leak ldrOperand2A (opA, adr);
  leak ldrOperand2B (opB, dst);
  leak ldrMemOperand (opR, val);
  leak ldrTransition (dst, val);

  opA <- adr;
  opB <- dst;
  opR <- val;
}

macro str3_leak (w32 val, w32 adr, w32 ofs)
{
  leak strOperand1 (opA, adr, opB, ofs);

  leak strOperand2A (opA, adr);
  leak strOperand2B (opB, val);
  leak strMemOperand (opW, val);

  opA <- adr;
  opB <- val;
  opW <- val;
}

macro mov2_leak (w32 dst, w32 src)
{
  // non-profiled assumptions without validation, warranty, whatsoever
  leak movCompResult (src);
  leak movOperand (opA, dst, opB, src);
  leak movTransition (dst, src);

  opA <- dst; // assumption
  opB <- src;
}

macro adds3_leak (w32 dst, w32 op1, w32 op2)
{
  // non-profiled assumptions without validation, warranty, whatsoever
  leak addsCompResult (op1 +w32 op2);
  leak addsTransition (dst, op1 +w32 op2);
  leak addsOperand (opA, op1, opB, op2);

  opA <- op1;
  opB <- op2;
}

macro add2_leak (w32 op1, w32 op2)
{
  // non-profiled assumptions without validation, warranty, whatsoever
  leak addCompResult (op1 +w32 op2);
  leak addTransition (op1, op1 +w32 op2);
  leak addOperand (opA, op1, opB, op2);

  opA <- op1;
  opB <- op2;
}

macro mvns2_leak (w32 dst, w32 src)
{
  // non-profiled assumptions without validation, warranty, whatsoever
  leak movCompResult (!w32 src);
  leak movOperand (opA, dst, opB, src);
  leak movTransition (dst, !w32 src);

  opA <- dst; // assumption
  opB <- src;
}
