OCAMLC=metaocamlc
TOP=metaocaml
OCAMLOPT=metaocamlopt

.SUFFIXES: .ml .mli .cmo .cmi .cmx .tex .pdf


LIBDIR=../lib
LIB=${LIBDIR}/stream.cma
LIBOPT=${LIBDIR}/stream.cmxa
INCLUDES=-I ../lib -I ../lib/backends/Trx -I ../lib/backends/C
# Order matters!

test: standard

.PHONY: standard
standard:
	${TOP} ${INCLUDES} ${LIB} test_stream.ml

.PHONY: window
window:
	${TOP} ${INCLUDES} ${LIB} test_window.ml

