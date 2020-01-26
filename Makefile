# Copyright 2019-2020 - Inria, NXP

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

ISASRCDIR := isa
ISASRC	  := $(wildcard $(ISASRCDIR)/*.il)
ISATESTS  := $(patsubst $(ISASRCDIR)/%.il, %.isatest, $(ISASRC))

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

# TODO, does not work beyond main
# .odoc file has no effect for unknown reasons
documentation:
	$(OCB) $(MAIN).odoc

logdir:
	mkdir -p $(LOGDIR)

# rule to test ilfiles
$(ILSRCDIR)/%.iltest %.iltest: $(ILSRCDIR)/%.il native logdir
	./scverif --il $< | tee $(LOGDIR)/$(notdir $@)

# rule to test instruction set architectures
$(ISASRCDIR)/%.isatest %.isatest: $(ISASRCDIR)/%.il native logdir
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

test-all: $(ILTESTS) $(ISATESTS) $(ASMTESTS) $(EVALTESTS)
