(* Copyright 2019-2020 - Inria, NXP *)
(* SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications *)

let version = "1.0.0"

let pp_copyright fmt = fun () -> Format.fprintf fmt
    "Copyright 2019-2020 - VeriSec Consortium, Inria@.@.\
     License: Modified BSD 3-Clause Clear@.@.\
     Redistribution and use in source and binary forms, with or without \
     modification, are permitted under any copyright provided that the \
     following conditions are met:@.@.\
     1. Redistributions of source code must retain the above copyright \
     notice, this list of conditions and the following disclaimer.@.\
     2. Redistributions in binary form must reproduce the above copyright \
     notice, this list of conditions and the following disclaimer in the \
     documentation and/or other materials provided with the distribution.@.\
     3. Neither the name of the copyright holder nor the names of its \
     contributors may be used to endorse or promote products derived from \
     this software without specific prior written permission.@.@.\
     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \
     \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT \
     LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, \
     NONINFRINGEMENT, AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. \
     IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR \
     ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL \
     DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE \
     GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS \
     INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER \
     IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR \
     OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN \
     IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."

let pp_mvatrb fmt = fun () -> Format.fprintf fmt
    "The software \"maskverif\" is used as a checker for probing security \
     and non-interference notions.@.\
     maskverif (https://gitlab.com/benjgregoire/maskverif)@.\
     Copyright (c) - 2016-2019 - Inria@.\
     Copyright (c) - 2016-2019 - X@.@.\
     Distributed under the terms of the CeCILL-B license.@.\
     https://cecill.info/licences/Licence_CeCILL-B_V1-en.txt"

(** whether the source files have diverged from git HEAD
    maintained by build system *)
let diverged : bool = false

(** git commit hash in case tree diverged from the "release" branch
    maintained by build system *)
let commithash : string option =
  None
