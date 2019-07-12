w32 opA;
w32 opB;

macro eors2_leak (w32 op1, w32 op2)
{
  leak eorsCompResult (op1, op2);
  leak eorsOperand (opA, op1, opB, op2);
  opA <- op1;
  opB <- op2;
}
