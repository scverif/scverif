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


macro ADDS (w32 rd, w32 rm, w32 rn)
   w32 aux
{ 
  aux <- rm +w32 rn;
  c   <- aux <w32 rm; // FIXME
  leak add (aux);
  rd <- aux;
}



   
  

