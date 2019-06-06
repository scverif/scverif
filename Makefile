.PHONY: all clean test

MENHIR          := menhir
MENHIRFLAGS     := --infer --explain
OCB_FLAGS	:= -tag bin_annot -I src -I lib -r -package batteries -package zarith -package menhirLib
OCB		:= ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)" $(OCB_FLAGS)  
MAIN            := main

UNAME_S   := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
	OCB_FLAGS += -lflags -cclib,-lrt
endif


all: native

native:
	$(OCB) -tag debug $(MAIN).native

clean:
	$(OCB) -clean
	rm -f src/*~ src/.*~ $(MAIN).native

test: all
	@echo "The following command should print 42.000000:"
	echo "(1 + 2 * 10) * 2" | ./$(MAIN).native

%.inferred.mli:
	@$(OCB) src/$@ && cat _build/src/$@c
