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
  leak ldrMemOperand (opR, val);
  leak ldrTransition (dst, val);
  leak ldrOperand (opB, val);
  opR <- val;
}

macro str3_leak (w32 val, w32 adr, w32 ofs)
{
  leak strMemOperand (opW, val);
  leak strOperand (opA, val);
  opW <- val;
}
