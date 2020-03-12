# Copyright 2019-2020 - Inria, NXP
# SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

.PHONY: all clean logdir
MAKEFLAGS += --silent

MENHIR       := menhir
MENHIRFLAGS  := --infer --explain
OCB_FLAGS    := -cflag -rectypes -r -docflags -rectypes,-html
OCB          := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" $(OCB_FLAGS)

MAIN         := main

UNAME_S      := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
	OCB_FLAGS += -lflags -cclib,-lrt
endif

LOGDIR    := logs

ILSRCDIR  := testil
ILSRC     := $(wildcard $(ILSRCDIR)/*.il)
ILTESTS   := $(patsubst $(ILSRCDIR)/%.il, %.iltest, $(ILSRC))

MODELSDIR	:= isa
MODELSSRC	:= $(wildcard $(MODELSDIR)/*.il)
MODELSTESTS	:= $(patsubst $(MODELSDIR)/%.il, %.isatest, $(MODELSSRC))

ASMSRCDIR := testasm
ASMSRC    := $(wildcard $(ASMSRCDIR)/*.objdump)
ASMILSRC  := $(wildcard $(ASMSRCDIR)/*.il)
ASMTESTS  := $(patsubst $(ASMSRCDIR)/%.il, %.asmtest, $(ASMILSRC))

EVALSRCDIR := testeval
EVALSRC    := $(wildcard $(EVALSRCDIR)/*.il)
EVALTESTS  := $(patsubst $(EVALSRCDIR)/%.il, %.evaltest, $(EVALSRC))

MVTESTS    := $(patsubst $(EVALSRCDIR)/%.il, %.mvtest, $(EVALSRC))


all: native

native:
	$(OCB) $(MAIN).native
	ln -fs $(MAIN).native scverif

byte:
	$(OCB) $(MAIN).byte
	ln -fs $(MAIN).byte scverif

debug:
	$(OCB) -tag debug $(MAIN).byte
	ln -fs $(MAIN).byte scverif

clean:
	$(OCB) -clean
	rm -f src/*~ src/.*~ $(MAIN).native $(LOGDIR)/*

container:
	docker build -t scverif -f .container/Dockerfile .

# TODO, does not work beyond main
# .odoc file has no effect for unknown reasons
documentation:
	$(OCB) $(MAIN).odoc

logdir:
	mkdir -p $(LOGDIR)

install: uninstall native
	@if [[ ":${PATH}:" == *":${HOME}/.local/bin:"* ]]; then\
	   mkdir -p "${HOME}/.local/bin/" && \
	   cp $(MAIN).native "${HOME}/.local/bin/scverif" && \
	   echo "installed scverif to '${HOME}/.local/bin/scverif'"; \
	elif [[ ":${PATH}:" == *":${HOME}/bin:"* ]]; then\
	   mkdir -p "${HOME}/bin/" && \
	   cp $(MAIN).native "${HOME}/bin/scverif" && \
	   echo "installed scverif to '${HOME}/bin/scverif'"; \
	else\
	  echo "Your path is missing ~/bin or ~/.local/bin, refusing to install executable.";\
	fi

uninstall:
ifneq (,$(wildcard ${HOME}/.local/bin/scverif))
	rm "${HOME}/.local/bin/scverif"
endif
ifneq (,$(wildcard ${HOME}/bin/scverif))
	rm "${HOME}/bin/scverif"
endif

# rule to test ilfiles
$(ILSRCDIR)/%.iltest %.iltest: $(ILSRCDIR)/%.il native logdir
	./scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# rule to test instruction set architectures
$(MODELSDIR)/%.isatest %.isatest: $(MODELSDIR)/%.il native logdir
	./scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# rule to test parsing of assembly files
$(ASMSRCDIR)/%.asmtest %.asmtest: $(ASMSRCDIR)/%.il native logdir
	./scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# rule to test ilfiles
$(EVALSRCDIR)/%.evaltest %.evaltest: $(EVALSRCDIR)/%.il native logdir
	./scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# shortcut for various tests
test-m0pisa: isa-cortex-m0plus.isatest

test-sxor: secxor-cortex-m0plus-O3.asmtest

test-sxors: secxor-cortex-m0plus-Os.asmtest

test-sand: secmult-cortex-m0plus-O3.asmtest

test-sands: secmult-cortex-m0plus-Os.asmtest

test-sref: secref-cortex-m0plus-O3.asmtest

test-srefs: secref-cortex-m0plus-Os.asmtest

test-il: secxor-while.iltest

tests: test-m0pisa test-sxor test-sxors test-sand test-sref test-srefs test-il

test-all: $(ILTESTS) $(MODELSTESTS) $(ASMTESTS) $(EVALTESTS)
