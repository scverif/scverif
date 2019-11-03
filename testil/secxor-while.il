// Copyright 2019 - Inria, NXP

w32 r0;
w32 r1;
w32 r2;
w32 r3;
w32 r4;
w32 r5;
w32 lr;
w32 sp;
w32 pc;

mem [];

macro movs_i (w32 r, int i)
{
   r <- (w32) i;
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

macro ldr (w32 d, w32 r, w32 ofs)
{
   d <- [w32 mem (int)(r +w32 ofs)];
}

macro eors (w32 d, w32 r)
{
  d <- d ^w32 r;
}

macro str(w32 r, w32 d, w32 ofs)
{
  [w32 mem (int)(d +w32 ofs)] <- d;
}

macro adds_i(w32 d, int i)
{
  d <- d +w32 (w32) i;
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

macro secxor ()
 label lbl1
 {
      movs_i(r3, 0);
      push3 (r4, r5, lr);
lbl1: ldr   (r4, r1, r3);
      ldr   (r5, r2, r3);
      eors  (r4, r5);
      str   (r4, r0, r3);
      adds_i(r3, 4);
      if (r3 != ((w32)12)) { goto lbl1; }
      pop3 (r4, r5, pc);
      goto pc;
}

annotate secxor
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 isp[0:2]
  init r1 [a 0]
  init r2 [b 0]
  init r0 [c 0]
  init sp [isp 12]
  init lr exit;

---
print:
  target: secxor
  kind: macrodef
  verbosity: 1;
...
