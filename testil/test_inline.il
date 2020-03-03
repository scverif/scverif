// Copyright 2019 - Inria, NXP
// SPDX-License-Identifier: BSD-3-Clause-Clear

mem [];
w32 r0;
w32 r1;
w32 r2;
w32 r3;
w32 r4;
w32 r5;
w32 r6;
w32 r7;
bool c;

macro OR3 (w32 x[1:3], w32 y[2:4], w32 z[0:2])
{
  z[0] <- x[1] |w32 y[2];
  z[1] <- x[2] |w32 y[3];
  z[2] <- x[3] |w32 y[4];
}

macro OOR3 (w32 a[0:8], w32 b[2:10])
{
  OR3(a[0:2], a[3:5], a[6:8]);
  OR3(b[2:4], b[5:7], b[8:10]);
}

macro OOOR3 (w32 a[0:8], w32 b[2:10])
{
  OOR3(b[2:10], a[0:8]);
  OOR3(a[0:8], b[2:10]);
}

macro CJUMP (bool b, label lbl)
{
  if b { goto lbl; }
}

macro loop (int n)
   label l1, label l2, int i
{
    i <- 0;
    goto l2;
l1: i <- i + 1;
l2: CJUMP (i < n, l1);
}
