/*
printf "include il \"testil/pseudo-secxor2.il\"\n" | ./main.native | ../maskverif/tool2/main_input.native
*/

include il "isa/pseudoisa.il"

verbose 0

/* declare your variables here at will
   or use the registers provided in pseudoisa.il
*/
w32 r42;

macro secxor ()
{
  load(r4, r1, 0);
  load(r5, r2, 0);
  xor(r4, r5);
  store(r4, r0, 0);

clear_all();

  load(r4, r1, 1);
  load(r5, r2, 1);
  xor(r5, r4);
  store(r4, r0, 1);

clear_all();

  load(r4, r1, 2);
  load(r5, r2, 2);
  xor(r4, r5);
  store(r4, r0, 2);
}

annotate secxor
  region mem w32 a[0:2]
  region mem w32 b[0:2]
  region mem w32 c[0:2]
  region mem w32 rnd[0:9]
  init r0 [c 0]
  init r1 [a 0]
  init r2 [b 0]
  init r3 [rnd 0]
  input sharing a
  input sharing b
  input urandom rnd
  output sharing c
  /*
    everything else assumed to be public input
    !and! public output for maskverif
    e.g. global "pub" variable
  */
  ;

print 4
  macro secxor;

apply accumulateleaks;    /* combine all leakage statements into a single statement */

print 4
  macro secxor;

verbose 4

apply inline;

print 4
  macro secxor;

apply partialeval
  secxor;

apply blacklistleaks      /* remove all leaks which are in the list below */
  secxor,                 /* 1st param is the name of the macro */
  xorOperandA,
  xorOperandB
  ;

/*
apply whitelistleaks      /* remove all leaks which are not in the list */
  secxor,                 /* 1st param is the name of the macro */
  xorCompResult,
  storeOperand,
  loadOperand,
  xorTransition,
  storeTransition,
  loadTransition
  ;*/

/* required to print maskverif */
apply deadcodeelim
  secxor;

print 0
  /*macro secxor*/
  /*maskverif secxor*/
  ;
