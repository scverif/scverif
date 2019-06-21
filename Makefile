.PHONY: all clean logdir

MENHIR       := menhir
MENHIRFLAGS  := --infer --explain
OCB_FLAGS    := -tag bin_annot -I src -r -package batteries -package zarith -package menhirLib -package ppx_deriving.show -package ppx_import
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
ASMTESTS  := $(patsubst $(ASMSRCDIR)/%.objdump, %.asmtest, $(ASMSRC))

EVALSRCDIR := testeval
EVALSRC    := $(wildcard $(EVALSRCDIR)/*.il)
EVALTESTS  := $(patsubst $(EVALSRCDIR)/%.il, %.evaltest, $(ISASRC))

all: native

native:
	$(OCB) -tag debug $(MAIN).native

clean:
	$(OCB) -clean
	rm -f src/*~ src/.*~ $(MAIN).native $(LOGDIR)/*

logdir:
	mkdir -p $(LOGDIR)

# rule to test ilfiles
%.iltest: $(ILSRCDIR)/%.il native logdir
	printf "read il $<\n" | ./$(MAIN).native | tee $(LOGDIR)/$@

# rule to test instruction set architectures
%.isatest: $(ISASRCDIR)/%.il native logdir
	printf "read il $<\n" | ./$(MAIN).native | tee $(LOGDIR)/$@

# rule to test parsing of assembly files
%.asmtest: $(ASMSRCDIR)/%.objdump native logdir
	printf "read il isa/isa-cortex-m0plus.il\nread asm $<\n" | ./$(MAIN).native | tee $(LOGDIR)/$@

# rule to test ilfiles
%.evaltest: $(EVALSRCDIR)/%.il native logdir
	printf "read il $<\n" | ./$(MAIN).native | tee $(LOGDIR)/$@


# shortcut for various tests
test-m0pisa: isa-cortex-m0plus.isatest

test-sxor: secxor-cortex-m0plus-O3.asmtest

test-sxors: secxor-cortex-m0plus-Os.asmtest

test-sand: secmult-cortex-m0plus-O3.asmtest

test-sands: secmult-cortex-m0plus-Os.asmtest

test-sref: secrefresh-cortex-m0plus-O3.asmtest

test-srefs: secrefresh-cortex-m0plus-Os.asmtest

test-il: secxor-while.iltest

tests: test-m0pisa test-sxor test-sxors test-sand test-sref test-srefs test-il

test-all: $(ILTESTS) $(ISATESTS) $(ASMTESTS) $(EVALTESTS)

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@c
