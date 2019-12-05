// Copyright 2019 - NXP

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
  opB <- ofs;

  leak ldrOperand2 (opA, adr, opB, val);
  leak ldrMemOperand (opR, val);
  leak ldrTransition (dst, val);

  opA <- adr;
  opB <- dst;
  opR <- val;
}

macro str3_leak (w32 val, w32 adr, w32 ofs)
{
  leak strOperand1 (opA, adr, opB, ofs);
  opB <- ofs;

  leak strOperand2 (opA, adr, opB, val);
  leak strMemOperand (opW, val);

  opA <- adr;
  opB <- val;
  opW <- val;
}

macro movs2_leak (w32 dst, w32 src)
{
  leak movsOperand (opA, dst, opB, src);
  leak movsTransition (dst, src);

  opA <- dst;
  opB <- src;
}
