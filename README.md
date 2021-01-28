[//1]: # (Copyright 2020-2021 - NXP)
[//2]: # (SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications)

## Introduction ##
scVerif allows to verify the side-channel resilience of executable
code in flexible models of side-channel behavior. It has been used
for, but is not limited to, probing security and threshold
non-interference notions with detailed models of power and
electromagnetic leakage of Arm Cortex-M micro-controllers. Both, the
leakage behavior and the semantic of code can be defined and adopted
by the user, allowing to express a large variety of side-channel
behavior for formal verification.

## Contributors ##
The software tool scVerif has been developed by the following
organizations and authors.

* VeriSec Consortium, consisting of
  * NXP Semiconductors
  * Ruhr University Bochum chair for embedded security
  * Ruhr University Bochum chair for security engineering
  * Technical University of Darmstadt chair for applied cryptography
  * TÜViT
* Inria

## License ##
This project is licensed under a modified BSD 3-Clause Clear License.
See the `LICENSE.txt` file in the root of this project.

## Installation (local) ##
1. Install the ocaml package manager "opam" according to [https://opam.ocaml.org/doc/Install.html](https://opam.ocaml.org/doc/Install.html).
2. Make sure `opam --version` reports a version >= 2.0.0.
3. Install [the maskverif SPINI branch](https://gitlab.com/benjgregoire/maskverif/tree/SPINI)
   - see the install description there.
4. run `opam pin add .` which install all dependencies
5. run examples via: `scverif --il testil/pseudo-secxor2.il`

## Installation (containerized) ##
1. run `make container`
2. run `docker run --rm -it scverif /bin/bash
3. run examples via:
4. Local folders can be mounted inside the container as follows:
   `docker run --rm -it -v <PathToFolderContainingIlFiles>:/home/opam/<YourDestinationName>` scverif /bin/bash`

## Acknowledgments ##
The research in this work was supported in part by the VeriSec project 16KIS0601K and 16KIS0634, from the Federal Ministry of Education and Research (BMBF).

The software `maskverif` is used as a checker for probing security and non-interference notions.
```
maskverif (https://gitlab.com/benjgregoire/maskverif)

Copyright (c) - 2016-2019 - Inria
Copyright (c) - 2016-2019 - X

Distributed under the terms of the CeCILL-B license.
https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt
```
