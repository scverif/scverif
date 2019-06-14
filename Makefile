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

test-asm: all
	echo "read asm test/secxor.objdump" | ./$(MAIN).native

test-asml: all
	echo "read asm test/secxor-loop.objdump" | ./$(MAIN).native

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@c
