# Copyright 2019-2020 - NXP
# SPDX-License-Identifier: BSD 3-Clause Clear License

FROM ocaml/opam2:4.07

ENV SCVHOME="/home/opam/scverif/"
ENV MVHOME="/home/opam/maskverif"
ENV MVBRANCH="SPINI"
ENV PATH=${PATH}:${HOME}/.local/bin

RUN mkdir -p ${HOME}/.local/bin \
        && mkdir -p ${SCVHOME}

RUN opam depext conf-m4 conf-gmp \
        && opam install ocamlfind ocamlbuild menhir \
        zarith ocamlgraph batteries ppx_deriving ppx_import re
RUN eval $(opam env)

# get and build maskverif
RUN eval $(opam env) \
        && git clone https://gitlab.com/benjgregoire/maskverif.git -b ${MVBRANCH} ${MVHOME} \
        && cd ${MVHOME} \
        && make clean install

# build local version of scverif
COPY --chown=opam . ${SCVHOME}
WORKDIR ${SCVHOME}
RUN eval $(opam env) \
        && make clean native

# make it available
ENV PATH="${SCVHOME}:${PATH}"
