[//1]: # (Copyright 2020 - NXP)
[//2]: # (SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications)

# Documentation #
Brief documentation for the available transformation passes and
necessary steps to analyze an executable, as well as adopting a
leakage model are given here. Please consider the examples in the
`testeval` directories as well as the comprehensive publication of
gadgets released in a separate repository. Please raise an issue for
guidance on the creation of execution models and adoption of the
source code.

## Commands and Transformations ##
scVerif is divided in two environments, a mode for specifying programs, state and annotations (`IL` mode) and a separate mode for instructing the framework to analyze programs (`SCV` commands).
The default environment is `IL` when running in interactive mode `scverif -i` or passing files via `scverif --il <file>`.

### IL commands ###
The language for specifying gadgets is described in a [paper](#TODO
insert link). Following `IL` commands are supported:

#### `include` ####
To include `IL`, `GAS` (gnu assembler) or `ASM` objdump files at point.
```
include il "models/leakyisa-cortex-m0plus.il"
include asm "testasm/secmult-cortex-m0plus-O3.objdump"
include gas "gnu-unified-assembler.s"
```
#### `var` ####
For declaring (typed) global variables. Possible types are `bool`, `w8,` (eight bits), `w16`, `w32`, `w64`, `int` (signed integers), `uint` (unsigned integers). Arrays of a specific type are declared by appending a range `[start:end]` where the integer "start" must be less or equal to "end", bounds are inclusive. Global memory is declare by an identifier without type and empty square brackets `<name>[]`.
```
<type> <varname>;

w32 r0;
int loopcounter;
bool flagOne;
memory[];
w32 table[-1:20];
```

#### `macro` ####
Macros are globally available functionality with typed parameters and
local variables. These are not "functions", i.e. variables outside the
scope can be accessed and parameters passed when calling macros can be
change by the called macro.

```
macro <name> (<type> <paramname>, <type> <paramname>, ...)
  <type> <localvariablename>, <type> <localvariablename>
{
  <insruction>;
}

macro noOperation () {}

macro mult (w32 a, w32 b)
  label loopstart
{
  loopstart:            // definition of label
  a <- a +w32 a;        // leakage free assignment, "+w32" addition of w32-typed variables
  leak(a, b);           // explicit leakage of a and b separately

  if (b != ((w32) 0)) { // "(w32) 0" is a cast if integer to w32
    goto loopstart;
  }
}
```

### Il instructions ###
- Assignment `<var> <- <expr>;`
- Leak `leak(<expr>,<expr>,...);`
- Label definition `<labelname>:`
- Macro call `<macroname>(<expr>, <expr>);`
- Static jump `goto <labelname>;`
- Indirect jump `goto <expr>;`

#### If conditional ####
Conditionals are available with optional false case.

```
if (<expr>) {
  <insruction>;
}

if (<expr>) {
  <insruction>;
} else {
  <insruction>
}
```

#### Loop ###
Conditional while.

```
while (<expr>) {
  <instruction>
}
```

Do-While variant

```
while {<instruction>} (<expr>)
```

### Il expressions ###
- constants `true`, `false`, `<integer>`, `0x<hex>`
- array access `<name>[<expr>]`
- memory access `[<type> <memoryname> <expr>]`
- equality of variable names `<expr> =name= <expr>`
- grouping `(<expr>)`
- cast `(<type>) <expr>`
- unary operations `<op1> <expr>`
- binary operations `<expr> <op1> <expr>`
- inline if `<expr> ? <expr> : <expr>`

#### Unary operations ####
- Arithmetic negation `-`
- logical negation `!`
- extensions `signextend (<type>)` and `zeroextend (<type>)`

#### Binary operations ####
- Addition `+<type>`
- Subtraction `-<type>`
- Exclusive or `^<type>`
- Conjunction `&<type>`
- Disjunction `|<type>`
- Multiplication (low half) `*<type>`
- Multiplication (high part) `**<type>`
- Logical shift `<<<type>`, `>><type>`
- Arithmetic shift right `>>s<type>`
- Equality `==<type>`
- Inequality `!=<type>`
- Less than `<<type>`, optionally signed `<s<type>`
- Less or equal `<=<type>`, optionally signed `<=s<type>`

#### `annotate` ####
Annotation of macros with an initial state and input/output behavior is necessary to conduct static-code analysis.

Four kinds of annotations of state and memory can be made:

1. Memory regions within a specific memory, declaring names of the regions at the same time.
2. Input or Output
3. Initialization of state
4. Security type (secret independent (`public`), masked (`sharing`), uniform random (`urandom`))

```
annotate examplegadget
  region mem w32 stack[0:10]       // memory "mem" contains a "stack" area of 11 positions with w32 type
  region mem w32 inpt[0:3]        // "mem" contains 4 input words in a region called "input"
  region mem w32 outpt[0:1]
  region mem w32 entropy[0:1]
  init metric_cyclecount 0         // specific value
  init r0 [inpt 0]                // r0 is a pointer to region "input" at offset 0
  init r1 [outpt 0]
  init r2 [entropy 0]
  init sp [stack 0]
  input public r0                  // r0 is a secret indepent input to "examplegadget"
  input public r1
  input public r2
  input public stack               // values in the "stack" region are secret independet inputs
  output sharing out outpt[0:1]   // the output named "out" is located in region "output"
  input sharing ina [in[0:1]]      // the masked input "ina" is a subpart of the "in" region
  input sharing inb [in[2], in[3]] // a list of subparts is possible
  input urandom entropy            // the region "entropy" contains randomness sampled from uniform
  ;
```

#### Comments ####
Using `//` for line-comments and `/*` to `*/` for regions.

### SCV Commands ###
The scv mode is entered via `---` and exited with `...`, similar to
YAML. In general the scv commands are build from dictionaries and
lists, where the top-level command must always be a dictionary with an
identifier corresponding to the transformation or action to be
applied.

### `print` ###
```
print:
  kind: []
  target:
  
  verbosity: [0-3] // 0 = off, 1 = normal, 2 = debug, 3 = full
```
### `addleakage` ###

### `inlinecall` ###

### `partialeval` ###

### `deadcodeelim` ###

### `check` ###

### `filter` ###

### `rewriteformaskverif` ###

### `accumulate` ###

### `verbosity` ###

## Analyzing an executable ##
The common process to analyze an executable is to `include` an execution model, a leakage model, the programs code (GAS, objdump, IL), specify an annotation (`annotate`) and subsequently switch to SCV mode using `---`.
Using `addleakage` the leakage is added to the execution model and the program.
For further analysis the program has to be flattened using `inlinecall` on the program to be analyzed.
If an annotation was given the `partialeval` pass produced an evaluated trace of the program which can finally be analyzed by the `check` pass.

## Adopting a leakage model ##
Two possibilities exist to model side-channel behavior of programs using scVerif and `IL`.
The leakage can be specified together with the semantic execution model as in `isa/pseudoisa.il` or the execution model is split from the leakage behavior as in `isa/isa-cortex-m0plus.il` and `isa/leakyisa-cortex-m0plus.il`.
Latter requires to include the leakage with the `addleakage` pass.
