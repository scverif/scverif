# Copyright 2019-2020 - Inria, NXP
# Copyright 2021 - NXP
# SPDX-License-Identifier: BSD-3-Clause-Clear WITH modifications

.PHONY: clean logdir build all
MAKEFLAGS += --silent

MAIN         := scverif

UNAME_S      := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
SHELL     := /bin/bash
endif

LOGDIR    := logs

ILSRCDIR  := testil
ILSRC     := $(wildcard $(ILSRCDIR)/*.il)
ILTESTS   := $(patsubst $(ILSRCDIR)/%.il, %.iltest, $(ILSRC))

MODELSDIR	:= models
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


all: build

build:
	dune build @install
	ln -sf scverif.exe $(MAIN)

clean:
	dune clean
	rm -f src/*~ src/.*~ $(MAIN) $(LOGDIR)/*

container:
	docker build -t scverif -f .container/Dockerfile .

# TODO, does not work beyond main
# .odoc file has no effect for unknown reasons
documentation:
	$(OCB) $(MAIN).odoc

logdir:
	mkdir -p $(LOGDIR)

# rule to test ilfiles
$(ILSRCDIR)/%.iltest %.iltest: $(ILSRCDIR)/%.il native logdir
	scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# rule to test instruction set architectures
$(MODELSDIR)/%.isatest %.isatest: $(MODELSDIR)/%.il native logdir
	scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# rule to test parsing of assembly files
$(ASMSRCDIR)/%.asmtest %.asmtest: $(ASMSRCDIR)/%.il native logdir
	scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# rule to test ilfiles
$(EVALSRCDIR)/%.evaltest %.evaltest: $(EVALSRCDIR)/%.il native logdir
	scverif --il $< | tee $(LOGDIR)/$(notdir $@)

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
