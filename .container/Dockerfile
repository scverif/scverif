# Copyright 2019-2020 - NXP
# SPDX-License-Identifier: BSD 3-Clause Clear License

FROM ocaml/opam2:4.07

RUN opam depext conf-m4 conf-gmp && opam install ocamlfind ocamlbuild menhir zarith ocamlgraph batteries ppx_deriving ppx_import re
RUN eval $(opam env)

RUN mkdir -p ${HOME}/.local/bin
ENV PATH=${PATH}:${HOME}/.local/bin

ENV MVHOME="/home/opam/maskverif"
#COPY --chown=opam maskverif ${MVHOME}
RUN eval $(opam env) && git clone https://gitlab.com/benjgregoire/maskverif.git -b SPINI ${MVHOME} && cd ${MVHOME}&& make clean install

ENV SCVHOME="/home/opam/scverif"
COPY --chown=opam scverif ${SCVHOME}
# TODO: pull from public git release
WORKDIR ${SCVHOME}
RUN eval $(opam env) && make
ENV PATH="${SCVHOME}:${PATH}"
