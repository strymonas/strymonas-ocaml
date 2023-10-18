OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLTOP=ocaml

.SUFFIXES: .ml .mli .cmo .cmi .cmx .tex .pdf

LIBDIR=../../lib
LIB=${LIBDIR}/stream_ocaml.cma
INCLUDES=-I ${LIBDIR} -I ${LIBDIR}/backends/Trx -I ${LIBDIR}/backends/C 

INTF=
OBJS=parameters.cmo fir.cmo stream_gnuradio_sdr_fn.cmo

# ========================================
.PHONY: all bench clean
all: bench

UNAME_OS=$(shell uname -s)
ifeq ($(UNAME_OS),Linux)
		PBCOPY=xsel --clipboard --input
else ifeq ($(UNAME_OS),Darwin)
		PBCOPY=pbcopy
endif
bench: ${OBJS}
	# echo "#ifndef CMPLXF" >| /tmp/generated.c
	# echo "#define CMPLXF(x, y) __builtin_complex ((float) (x), (float) (y))" >> /tmp/generated.c
	# echo "#endif" >> /tmp/generated.c
	$(OCAMLTOP) $(INCLUDES) $(LIB) $^ main.ml #| tail -n +2 >> /tmp/generated.c


.mli.cmi:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmo:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmx:
	$(OCAMLOPT) -c $(INCLUDES) $<

# depend: $(wildcard *.mli *.ml)
# 	$(OCAMLDEP) $(INCLUDES) $^ > depend

clean::
	rm -f *.cm[ixoa] *.cmxa *.[oa] *.out depend

test_atan:
	$(CC) -W -Wall -O3 -ffast-math -lm atan_test.c && ./a.out

-include depend
