OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
METAOCAMLC=metaocamlc
METAOCAMLOPT=metaocamlopt
METAOCAMLTOP=metaocaml

.SUFFIXES: .ml .mli .cmo .cmi .cmx .tex .pdf

LIBDIR=../../lib
LIB=${LIBDIR}/stream.cma
LIBOPT=${LIBDIR}/stream.cmxa
LIBPURE=${LIBDIR}/stream_ocaml.cma
LIBPUREOPT=${LIBDIR}/stream_ocaml.cmxa
INCLUDES=-I ${LIBDIR} -I ${LIBDIR}/backends/Trx -I ${LIBDIR}/backends/C 

INTF=
OBJS=util.cmo parameters.cmo fir.cmo stream_streamit_sdr_fn.cmo
# ========================================
.PHONY: all bench test test_c clean
all: bench

UNAME_OS=$(shell uname -s)
ifeq ($(UNAME_OS),Linux)
		PBCOPY=xsel --clipboard --input
else ifeq ($(UNAME_OS),Darwin)
		PBCOPY=pbcopy
endif
# Only main requires MetaOCaml
bench: ${OBJS}
	$(METAOCAMLTOP) $(INCLUDES) $(LIB) $^ main.ml | tail -n +3 #| $(PBCOPY)

test: ${OBJS} sit_emulator.cmo sit_experiments.cmo 
	$(METAOCAMLC) -o test.out $(INCLUDES) $(LIB) $^  test.ml && ./test.out
# Pure OCaml
test_c: $(OBJS:.cmo=.cmx) sit_emulator.cmx sit_experiments.cmx 
	$(OCAMLOPT) -o test_c.out $(INCLUDES) $(LIBPUREOPT) $^ test_c.ml && ./test_c.out

# None of the OBJS requuire MetaOCaml

.mli.cmi:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmo:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmx:
	$(OCAMLOPT) -c $(INCLUDES) $<

depend: $(wildcard *.mli *.ml)
	$(OCAMLDEP) $(INCLUDES) $^ > depend

clean::
	rm -f *.cm[ixoa] *.cmxa *.[oa] *.out depend

-include depend
