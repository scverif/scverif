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
macro AddWithCarry(w32 x, w32 y, bool carry_in, w32 result, bool carry_out, bool overflow)
  int unsigned_sum, int signed_sum
{
  unsigned_sum <- (uint) x + (uint) y + (uint) carry_in;
  signed_sum   <- (int) x  + (int) y  + (uint) carry_in;
  result       <- (w32) unsigned_sum;
  carry_out    <- ! ((uint) result == unsigned_sum);
  overflow     <- ! ((int)  result ==   signed_sum);
}

macro IsZero(w32 x, bool r)
{
  r <- (uint) x == 0;
}

macro ExtractBit(w32 x, int i, bool r)
/* counting from zero */
{
  r <- (x >>w32 i) ==w32 (w32) 1;
}

macro Not (w32 op, w32 r)
/* Complement of bitstring  */
{
  r <- op ^w32 (w32) -1;
}

/************** INSTRUCTIONS **************/


macro mov2 (w32 r, w32 i)
{
   r <- i;
}

macro movs2 (w32 r, w32 i)
{
   mov2(r, i);
   apsrn <- (i >>w32 31) ==w32 (w32) 1;
   apsrz <- i ==w32 (w32) 0;
}

macro push1 (w32 r)
{
   sp <- sp -w32 ((w32) 4);
   [w32 mem (int)sp] <- r;
}

macro push2 (w32 r1, w32 r2)
{
   sp <- sp -w32 ((w32) 8);
   [w32 mem ((int)sp) + 0] <- r1;
   [w32 mem ((int)sp) + 4] <- r2;
}

macro push3 (w32 r1, w32 r2, w32 r3)
{
   sp <- sp -w32 ((w32) 12);
   [w32 mem ((int)sp) + 0] <- r1;
   [w32 mem ((int)sp) + 4] <- r2;
   [w32 mem ((int)sp) + 8] <- r3;
}

macro push4 (w32 r1, w32 r2, w32 r3, w32 r4)
{
   sp <- sp -w32 ((w32) 16);
   [w32 mem ((int)sp) + 0]  <- r1;
   [w32 mem ((int)sp) + 4]  <- r2;
   [w32 mem ((int)sp) + 8]  <- r3;
   [w32 mem ((int)sp) + 12] <- r4;
}

macro push5 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5)
{
   sp <- sp -w32 ((w32) 20);
   [w32 mem ((int)sp) + 0]  <- r1;
   [w32 mem ((int)sp) + 4]  <- r2;
   [w32 mem ((int)sp) + 8]  <- r3;
   [w32 mem ((int)sp) + 12] <- r4;
   [w32 mem ((int)sp) + 16] <- r5;
}

macro push6 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6)
{
   sp <- sp -w32 ((w32) 24);
   [w32 mem ((int)sp) + 0]  <- r1;
   [w32 mem ((int)sp) + 4]  <- r2;
   [w32 mem ((int)sp) + 8]  <- r3;
   [w32 mem ((int)sp) + 12] <- r4;
   [w32 mem ((int)sp) + 16] <- r5;
   [w32 mem ((int)sp) + 20] <- r6;
}

macro push7 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6, w32 r7)
{
   sp <- sp -w32 ((w32) 28);
   [w32 mem ((int)sp) + 0]  <- r1;
   [w32 mem ((int)sp) + 4]  <- r2;
   [w32 mem ((int)sp) + 8]  <- r3;
   [w32 mem ((int)sp) + 12] <- r4;
   [w32 mem ((int)sp) + 16] <- r5;
   [w32 mem ((int)sp) + 20] <- r6;
   [w32 mem ((int)sp) + 24] <- r7;
}

macro push8 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6, w32 r7, w32 r8)
{
   sp <- sp -w32 ((w32) 32);
   [w32 mem ((int)sp) + 0]  <- r1;
   [w32 mem ((int)sp) + 4]  <- r2;
   [w32 mem ((int)sp) + 8]  <- r3;
   [w32 mem ((int)sp) + 12] <- r4;
   [w32 mem ((int)sp) + 16] <- r5;
   [w32 mem ((int)sp) + 20] <- r6;
   [w32 mem ((int)sp) + 24] <- r7;
   [w32 mem ((int)sp) + 28] <- r8;
}

macro push9 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6, w32 r7, w32 r8, w32 r9)
{
   sp <- sp -w32 ((w32) 36);
   [w32 mem ((int)sp) + 0]  <- r1;
   [w32 mem ((int)sp) + 4]  <- r2;
   [w32 mem ((int)sp) + 8]  <- r3;
   [w32 mem ((int)sp) + 12] <- r4;
   [w32 mem ((int)sp) + 16] <- r5;
   [w32 mem ((int)sp) + 20] <- r6;
   [w32 mem ((int)sp) + 24] <- r7;
   [w32 mem ((int)sp) + 28] <- r8;
   [w32 mem ((int)sp) + 32] <- r9;
}

macro pop1 (w32 r)
{
   r <- [w32 mem (int)sp];
   sp <- sp +w32 ((w32) 4);
}

macro pop2 (w32 r1, w32 r2)
{
   r1 <- [w32 mem ((int)sp + 0)];
   r2 <- [w32 mem ((int)sp + 4)];
   sp <- sp +w32 ((w32) 8);
}

macro pop3 (w32 r1, w32 r2, w32 r3)
{
   r1 <- [w32 mem ((int)sp) + 0];
   r2 <- [w32 mem ((int)sp) + 4];
   r3 <- [w32 mem ((int)sp) + 8];
   sp <- sp +w32 ((w32) 12);
}

macro pop4 (w32 r1, w32 r2, w32 r3, w32 r4)
{
   r1 <- [w32 mem ((int)sp) + 0];
   r2 <- [w32 mem ((int)sp) + 4];
   r3 <- [w32 mem ((int)sp) + 8];
   r4 <- [w32 mem ((int)sp) + 12];
   sp <- sp +w32 ((w32) 16);
}

macro pop5 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5)
{
   r1 <- [w32 mem ((int)sp) + 0];
   r2 <- [w32 mem ((int)sp) + 4];
   r3 <- [w32 mem ((int)sp) + 8];
   r4 <- [w32 mem ((int)sp) + 12];
   r5 <- [w32 mem ((int)sp) + 16];
   sp <- sp +w32 ((w32) 20);
}

macro pop6 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6)
{
   r1 <- [w32 mem ((int)sp) + 0];
   r2 <- [w32 mem ((int)sp) + 4];
   r3 <- [w32 mem ((int)sp) + 8];
   r4 <- [w32 mem ((int)sp) + 12];
   r5 <- [w32 mem ((int)sp) + 16];
   r6 <- [w32 mem ((int)sp) + 20];
   sp <- sp +w32 ((w32) 24);
}

macro pop7 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6, w32 r7)
{
   r1 <- [w32 mem ((int)sp) + 0];
   r2 <- [w32 mem ((int)sp) + 4];
   r3 <- [w32 mem ((int)sp) + 8];
   r4 <- [w32 mem ((int)sp) + 12];
   r5 <- [w32 mem ((int)sp) + 16];
   r6 <- [w32 mem ((int)sp) + 20];
   r7 <- [w32 mem ((int)sp) + 24];
   sp <- sp +w32 ((w32) 28);
}

macro pop8 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6, w32 r7, w32 r8)
{
   r1 <- [w32 mem ((int)sp) + 0];
   r2 <- [w32 mem ((int)sp) + 4];
   r3 <- [w32 mem ((int)sp) + 8];
   r4 <- [w32 mem ((int)sp) + 12];
   r5 <- [w32 mem ((int)sp) + 16];
   r6 <- [w32 mem ((int)sp) + 20];
   r7 <- [w32 mem ((int)sp) + 24];
   r8 <- [w32 mem ((int)sp) + 28];
   sp <- sp +w32 ((w32) 32);
}

macro pop9 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6, w32 r7, w32 r8, w32 r9)
{
   r1 <- [w32 mem ((int)sp) + 0];
   r2 <- [w32 mem ((int)sp) + 4];
   r3 <- [w32 mem ((int)sp) + 8];
   r4 <- [w32 mem ((int)sp) + 12];
   r5 <- [w32 mem ((int)sp) + 16];
   r6 <- [w32 mem ((int)sp) + 20];
   r7 <- [w32 mem ((int)sp) + 24];
   r8 <- [w32 mem ((int)sp) + 28];
   r8 <- [w32 mem ((int)sp) + 32];
   sp <- sp +w32 ((w32) 36);
}

macro ldr3 (w32 d, w32 r, w32 ofs)
{
   d <- [w32 mem (int)(r +w32 ofs)];
}

macro str3 (w32 r, w32 d, w32 ofs)
{
  /* writes 4 bytes per address, not equal to w32 array */
  [w32 mem (int)(d +w32 ofs)] <- r;
}

macro lsls3 (w32 r1, w32 r2, w32 imm5)
{
  if(imm5 !=w32 (w32) 0) {
    apsrc <- (r2 >>w32 (int) ((w32) 32 -w32 imm5)) ==w32 (w32) 1;
    /* depends on semantic of <<w32 */
    r1 <- r2 <<w32 (int) imm5;
    /* computes on new r1 value */
    apsrn <- (r1 >>w32 31) ==w32 (w32) 1;
    apsrz <- r1 ==w32 (w32) 0;
  }
}

macro lsls2 (w32 r1, w32 imm5)
{
  lsls3 (r1, r1, imm5);
}

macro eors2 (w32 r1, w32 r2)
{
  r1 <- r1 ^w32 r2;
  /* computes on new r1 value */
  apsrn <- (r1 >>w32 31) ==w32 (w32) 1;
  apsrz <- r1 ==w32 (w32) 0;
}

macro ands2 (w32 r1, w32 r2)
{
  r1 <- r1 &w32 r2;
  /* computes on new r1 value */
  apsrn <- (r1 >>w32 31) ==w32 (w32) 1;
  apsrz <- r1 ==w32 (w32) 0;
}

macro adds3 (w32 d, w32 r1, w32 r2)
{
  AddWithCarry(r1, r2, false, d, apsrc, apsrv);
  ExtractBit(d, 31, apsrn);
  IsZero(d, apsrz);
}

macro adds2 (w32 r1, w32 r2)
{
  adds3(r1, r1, r2);
}

macro add3 (w32 d, w32 r1, w32 r2)
  bool i, bool j
{
  AddWithCarry(r1, r2, false, d, i, j);
}

macro add2 (w32 r1, w32 r2)
{
  add3(r1, r1, r2);
}

macro sub3 (w32 d, w32 r1, w32 r2)
  bool i, bool j
{
  Not(r2, r2);
  AddWithCarry(r1, r2, true, d, i, j);
}

macro sub2 (w32 op1, w32 op2)
{
  sub3(op1, op1, op2);
}

macro cmp2 (w32 r1, w32 i)
{
  apsrn <- ((r1 +w32 i) >>w32 31) ==w32 (w32) 1;
  apsrz  <- ((r1 +w32 i) ==w32 (w32) 0);
  apsrc <- ((r1 >>w32 31) &w32 (i >>w32 31)) ==w32 (w32) 1;
  /* TODO overflow (apsrv) missing */
}

macro bn1 (label l)
{
   /*BEWARE: apsrv not implemented*/
   if (!apsrv) { goto l; }
}

macro bnen1 (label l)
{
   if (!apsrz) { goto l; }
}