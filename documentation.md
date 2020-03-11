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

#### `annotate` ####
To annotate a global macro with initial state and input/output behavior.

#### `macro` ####
For defining globally available functionality with typed parameters and local variables.

#### `var` ####
For declaring (typed) global variables.

#### Comments ####
Using `//` for line-comments and `/*` to `*/` for regions.

### SCV Commands ###
The scv mode is entered via `---` and exited with `...`, similar to YAML.

### `addleakage` ###

### `print` ###

### `inlinecall` ###

### `partialeval` ###

### `deadcodeelim` ###

### `check` ###

### `filter` ###

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
