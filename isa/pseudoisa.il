// Copyright 2019 - NXP

/* infinite amount of variables/registers */

/* globals */
w32 pub;
mem [];

/* call by-reference calling convention */
w32 r0; /* pointer for output shares c   */
w32 r1; /* pointer for input shares  a   */
w32 r2; /* pointer for input shares  b   */
w32 r3; /* pointer for entropy       rnd */

/* these registers are available on hardware */
w32 r4;
w32 r5;
w32 r6;
w32 r7;

/* but these cannot be used for computation
w32 r8;
w32 r9;
w32 r10;
w32 r11;
w32 r12;
*/

/* variables to capute the leakage state */
/* do not use for computation! */
w32 opA; /* Computation Operand 1 Transition */
w32 opB; /* Computation Operand 2 Transition */
w32 opR; /* Memory Read Operand Transition */
w32 opW; /* Memory Write Operand Transition */

/* basic instruction set */

macro xor (w32 op1, w32 op2)
{
  leak xorCompResult (op1 ^w32 op2);
  leak xorOperand (opA, op1, opB, op2);
  leak xorOperandA (opA, op1);
  leak xorOperandB (opB, op2);
  leak xorTransition (op1, op1 ^w32 op2);
  opA <- op1;
  opB <- op2;
  op1 <- op1 ^w32 op2;
}

macro and (w32 op1, w32 op2)
{
  leak andCompResult (op1 &w32 op2);
  leak andOperand (opA, op1, opB, op2);
  leak andOperandA (opA, op1);
  leak andOperandB (opB, op2);
  leak andTransition (op1, op1 &w32 op2);
  opA <- op1;
  opB <- op2;
  op1 <- op1 &w32 op2;
}

/* index is an offset in units of 32bit */
macro load (w32 dst, w32 ptr, int index)
  w32 val
{
  val <- [w32 mem (int) (ptr +w32 (w32) (index * 4))];
  leak loadMemOperand (opR, val);
  leak loadTransition (dst, val);
  leak loadOperand (opB, val); /* Needs confirmation */
  opR <- val;
  opA <- val;
  dst <- val;
}

/* index is an offset in units of 32bit */
macro store (w32 src, w32 ptr, int index)
{
  leak storeMemOperand (opW, src);
  leak storeOperand (opA, src);
  opA <- src; /* Needs confirmation */
  opW <- src;
  [w32 mem (uint) (ptr +w32 (w32) (index * 4))] <- src;
}

macro clear_opA ()
{
  opA <- pub;
}

macro clear_all ()
{
  opA <- pub;
  opB <- pub;
  opR <- pub;
  opW <- pub;
}
