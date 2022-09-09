# Test the simplest examples
include ../Makefile.common

# Order matters!

.PHONY: all
all: trytop c

trytop:
	${TOP} ${INCLUDES} ${LIB} simple.ml
	echo '#use "simple.ml";;' | ${TOP}

# We want to demonstrate pure OCaml, so we manually pick up only relevant
# pure OCaml modules
c:
	$(OCAML) ${INCLUDES} ${LIBPURE} simple_c.ml
