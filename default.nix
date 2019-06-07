with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "mytool-0";
  src = ./.;
  buildInputs = [ ]
    ++ (with ocamlPackages; [ ocaml findlib ocamlbuild menhir zarith merlin ocamlgraph batteries ppx_deriving])
    ;
}
