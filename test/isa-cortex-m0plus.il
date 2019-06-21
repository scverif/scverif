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

bool zflag;
bool vflag;

mem [];

macro mov2 (w32 r, w32 i)
{
   r <- i;
}

macro movs2 (w32 r, w32 i)
{
   r <- i;
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
   sp <- sp -w32 ((w32) 12);
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
   sp <- sp -w32 ((w32) 20);
   [w32 mem ((int)sp) + 0]  <- r1;
   [w32 mem ((int)sp) + 4]  <- r2;
   [w32 mem ((int)sp) + 8]  <- r3;
   [w32 mem ((int)sp) + 12] <- r4;
   [w32 mem ((int)sp) + 16] <- r5;
   [w32 mem ((int)sp) + 20] <- r6;
}

macro push7 (w32 r1, w32 r2, w32 r3, w32 r4, w32 r5, w32 r6, w32 r7)
{
   sp <- sp -w32 ((w32) 20);
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
   sp <- sp -w32 ((w32) 32);
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
   [w32 mem ((int)sp) + 0] <- r1;
   [w32 mem ((int)sp) + 4] <- r2;
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
  [w32 mem (int)(d +w32 ofs)] <- d;
}

macro lsls2 (w32 r1, w32 r2)
{
  r1 <- r1 <<w32 (int) r2;
}

macro eors2 (w32 d, w32 r)
{
  d <- d ^w32 r;
}

macro and2 (w32 d, w32 r)
{
  d <- d &w32 r;
}

macro ands2 (w32 d, w32 r)
{
  d <- d &w32 r;
}

macro adds2 (w32 d, w32 i)
{
  d <- d +w32 i;
}

macro adds3 (w32 d, w32 r1, w32 r2)
{
  d <- r1 +w32 r2;
}

macro sub2 (w32 d, w32 i)
{
  d <- d -w32 i;
}

macro cmp2 (w32 r1, w32 i)
{
   zflag <- (r1 ==w32 i);
}

macro bn1 (label l)
{
   if (vflag) { goto l; }
}

macro bnen1 (label l)
{
   if (!zflag) { goto l; }
}