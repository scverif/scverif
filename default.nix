# Copyright 2019-2020 - Inria, NXP
# SPDX-License-Identifier: BSD-3-Clause-Clear

with import <nixpkgs> {};

let maskverif = import ../maskverif
;in

stdenv.mkDerivation {
  name = "scverif-0";
  src = ./.;
  buildInputs = [ ]
    ++ (with ocamlPackages; [ ocaml findlib ocamlbuild menhir zarith merlin ocamlgraph batteries ppx_deriving ppx_import re maskverif])
    ;
}
