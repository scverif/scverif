w32 opA;
w32 opB;
w32 opR;
w32 opW;

macro eors2_leak (w32 op1, w32 op2)
{
  leak eorsCompResult (op1 ^w32 op2);
  leak eorsOperand (opA, op1, opB, op2);
  leak eorsOperandA (opA, op1);
  leak eorsOperandB (opB, op2);
  opA <- op1;
  opB <- op2;
}

macro ands2_leak (w32 op1, w32 op2)
{
  leak andsCompResult (op1, op2);
  leak andsOperand (opA, op1, opB, op2);
  leak andsOperandA (opA, op1);
  leak andsOperandB (opB, op2);
  opA <- op1;
  opB <- op2;
}

macro ldr3_leak (w32 dst, w32 adr, w32 ofs)
{
  leak memoryOperand (opR, opW, [w32 mem (int) (adr +w32 ofs)]);
  opR <- [w32 mem (int) (adr +w32 ofs)];
}

macro str3_leak (w32 op, w32 adr, w32 ofs)
{
  leak memoryOperand (opR, opW, op);
  opW <- op;
}
