/*
printf "include il \"testil/pseudo-secxor2.il\"\n" | ./main.native | ../maskverif/tool2/main_input.native
*/

include il "isa/pseudoisa.il"

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
annotate:
  secxor:
    region:
      mem:
        a: w32[0:2]
        b: w32[0:2]
        c: w32[0:2]
        rnd: w32[0:9];;
    init:
      r0: c[0]
      r1: a[0]
      r2: b[0]
      r3: rnd[0];
    input:
      sharing: [a, b]
      urandom: rnd;
    output:
      sharing: [c];
  ;
    everything else assumed to be public input
    !and! public output for maskverif
    e.g. global "pub" variable
  */
  ;

---
accumulate:
  target: secxor
  keep: true;
...


/* bigger example
---
addleakage:
  targets: secxor
  restrict: "*transition*";
accumulate:
  targets: [secxor, load, store]
  keepleaks: true
  leakages: [xorCompResult, xorOperandA];
inline:
  targets: any;
deadcodeelim:
  targets: secxor
  Print: y;
check:
  targets: secxor
  algorithm:
    type: NI
    order: 2;
  Print: debug;
...
*/
/* example list (target macros, no params) -> not allowed as toplevel
---
[secxor, secand, secload]
...*/

/* example singleton (target macro, no params) -> not allowed as toplevel
---
secxor
...*/
