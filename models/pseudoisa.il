// Copyright 2019-2020 - NXP, TU Darmstadt
// SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

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

/* but these should not be used for computation */
w32 r8;
w32 r9;
w32 r10;
w32 r11;
w32 r12;
w32 sp;

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

macro copy (w32 dst, w32 src)
{
  // non-profiled assumptions without validation, warranty, whatsoever
  leak movCompResult (src);
  leak movOperand (opA, dst, opB, src);
  leak movTransition (dst, src);

  opA <- dst; // assumption
  opB <- src;

  dst <- src;
}

/* index is an offset in units of 32bit */
macro load (w32 dst, w32 ptr, int index)
  w32 val
{
  val <- [w32 mem (int) (ptr +w32 (w32) (index * 4))];

  leak loadOperand1 (opA, ptr, opB, index);

  leak loadOperand2A (opA, ptr);
  leak loadOperand2B (opB, dst);
  leak loadMemOperand (opR, val);
  leak loadTransition (dst, val);

  opA <- ptr;
  opB <- dst;
  opR <- val;

  dst <- val;
}

/* index is an offset in units of 32bit */
macro store (w32 src, w32 ptr, int index)
{
  leak storeOperand1 (opA, ptr, opB, index);

//  leak storeOperand2WC (opA, ptr, opB, dst, opW, src); /* extreme worst case not yet reported */
//  leak storeOperand2 (opA, ptr, opB, dst);             /* worst case not yet reported */
  leak storeOperand2A (opA, ptr);
  leak storeOperand2B (opB, src);
  leak storeMemOperand (opW, src);

  opA <- ptr;
  opB <- src;
  opW <- src;

  [w32 mem (uint) (ptr +w32 (w32) (index * 4))] <- src;
}
