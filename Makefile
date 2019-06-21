.PHONY: all clean test

MENHIR       := menhir
MENHIRFLAGS  := --infer --explain
OCB_FLAGS    := -tag bin_annot -I src -r -package batteries -package zarith -package menhirLib -package ppx_deriving.show -package ppx_import
OCB          := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" $(OCB_FLAGS)

MAIN         := main

UNAME_S      := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	OCB_FLAGS += -lflags -cclib,-lrt
endif


all: native

native:
	$(OCB) -tag debug $(MAIN).native

clean:
	$(OCB) -clean
	rm -f src/*~ src/.*~ $(MAIN).native

test-m0pisa: all
	printf "read il isa/isa-cortex-m0plus.il" | ./$(MAIN).native

test-sxor: all
	printf "read il isa/isa-cortex-m0plus.il\nread asm testasm/secxor-cortex-m0plus-O3.objdump\n" | ./$(MAIN).native

test-sxors: all
	printf "read il isa/isa-cortex-m0plus.il\nread asm testasm/secxor-cortex-m0plus-Os.objdump\n" | ./$(MAIN).native

test-sand: all
	printf "read il isa/isa-cortex-m0plus.il\nread asm testasm/secmult-cortex-m0plus-O3.objdump\n" | ./$(MAIN).native

test-sands: all
	printf "read il isa/isa-cortex-m0plus.il\nread asm testasm/secmult-cortex-m0plus-Os.objdump\n" | ./$(MAIN).native

test-sref: all
	printf "read il isa/isa-cortex-m0plus.il\nread asm testasm/secrefresh-cortex-m0plus-O3.objdump\n" | ./$(MAIN).native

test-srefs: all
	printf "read il isa/isa-cortex-m0plus.il\nread asm testasm/secrefresh-cortex-m0plus-Os.objdump\n" | ./$(MAIN).native

test-il: all
	printf "read il testil/secxor-while.il\n" | ./$(MAIN).native

tests: all test-m0pisa test-sxor test-sxors test-sand test-sref test-srefs test-il

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@c
