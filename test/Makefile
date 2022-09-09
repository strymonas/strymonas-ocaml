# Some tests are MetaOCaml-specific
TOP=metaocaml
OCAMLC=metaocamlc
OCAMLOPT=metaocamlopt

.SUFFIXES: .ml .mli .cmo .cmi .cmx


LIBDIR=../lib
LIB=${LIBDIR}/stream.cma
LIBOPT=${LIBDIR}/stream.cmxa
INCLUDES=-I ../lib -I ../lib/backends/Trx -I ../lib/backends/C
# Order matters!

test: standard

.PHONY: standard
standard:
	${TOP} ${INCLUDES} ${LIB} test_stream.ml

