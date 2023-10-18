OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
# Some facilities require MetaOCaml
METAOCAMLC=metaocamlc
METAOCAMLOPT=metaocamlopt

.SUFFIXES: .ml .mli .cmo .cmi .cmx .tex .pdf

LIB=stream.cma
LIBOPT=stream.cmxa
# Pure OCaml, not MetaOCaml
LIBPURE=stream_ocaml.cma
LIBPUREOPT=stream_ocaml.cmxa

# Trx backend needs MetaOCaml; the others are pure OCaml
BACKC=backends/C
BACKTRX=backends/Trx
BACKENDC=\
   $(BACKC)/c_ast.cmi \
   $(BACKC)/c_pp.cmo \
   $(BACKC)/offshoringIR.cmi \
   $(BACKC)/offshoringIR.cmo \
   $(BACKC)/offshoringIR_pp.cmo \
   $(BACKC)/c_cde.cmo
BACKENDS=\
  $(BACKENDC) \
  $(BACKTRX)/trx_code_common.cmo

# Order matters!
INTF=cde.cmi cde_ex.cmi cde_top.cmi stream_raw.cmi stream_cooked.cmi 
# OBJS are pure OCaml objects
OBJS=pk_cde.cmo pk_coll.cmo stream_raw_fn.cmo stream_cooked_fn.cmo window_fn.cmo
INCLUDES=-I $(BACKC) -I $(BACKTRX)

.PHONY: all
all: lib opt pure
lib: $(LIB)
opt: $(LIBOPT) $(LIBPUREOPT)
pure: $(LIBPURE)

.PHONY: intf			#Just the interfaces for testing examples
intf: $(INTF)

$(LIB): $(INTF) $(OBJS) $(BACKENDS) $(BACKTRX)/trx_code.cmo backends.cmo
	$(OCAMLC) -a -o $@ $(filter-out %.cmi,$^)

$(LIBOPT): $(INTF) $(BACKENDS:.cmo=.cmx) $(OBJS:.cmo=.cmx) \
	$(BACKTRX)/trx_code_native.cmx backends.cmx
	$(OCAMLOPT) -a -o $@ $(filter-out %.cmi,$^)

$(LIBPURE): $(INTF) $(BACKENDC) $(OBJS)
	$(OCAMLC) -a -o $@ $(filter-out %.cmi,$^)

$(LIBPUREOPT): $(INTF) $(BACKENDC:.cmo=.cmx) $(OBJS:.cmo=.cmx) 
	$(OCAMLOPT) -a -o $@ $(filter-out %.cmi,$^)


.mli.cmi:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmo:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmx:
	$(OCAMLOPT) -c $(INCLUDES) $<


$(BACKTRX)/trx_code_common.cmo: $(BACKTRX)/trx_code_common.ml
	$(METAOCAMLC) -c  $<

$(BACKTRX)/trx_code.cmo: $(BACKTRX)/trx_code.ml
	$(METAOCAMLC) -c -I $(BACKTRX) $<

$(BACKTRX)/trx_code_common.cmx: $(BACKTRX)/trx_code_common.ml
	$(METAOCAMLOPT) -c  $<

$(BACKTRX)/trx_code_native.cmx: $(BACKTRX)/trx_code_native.ml
	$(METAOCAMLOPT) -c -I $(BACKTRX) $<

$(BACKC)/offshoringIR_pp.cmo: $(BACKC)/offshoringIR_pp.ml $(BACKC)/c_ast.cmi
	$(OCAMLC) -c  $(INCLUDES) -no-alias-deps $(BACKC)/offshoringIR_pp.ml

$(BACKC)/offshoringIR_pp.cmx: $(BACKC)/offshoringIR_pp.ml $(BACKC)/c_ast.cmi
	$(OCAMLOPT) -c  $(INCLUDES) -no-alias-deps $(BACKC)/offshoringIR_pp.ml

backends.cmo: backends.ml
	$(METAOCAMLC) -c  $(INCLUDES) $<
backends.cmx: backends.ml
	$(METAOCAMLOPT) -c  $(INCLUDES) $<

depend: $(wildcard *.mli *.ml $(BACKC)/*.{ml,mli} $(BACKTRX)/*.{ml,mli})
	$(OCAMLDEP) $(INCLUDES) $^ > depend

clean::
	rm -f *.cm[ixoa] *.cmxa *.[oa] depend

clean::
	cd $(BACKC) && rm -f *.cm[ixoa] *.cmxa *.[oa]

clean::
	cd $(BACKTRX) && rm -f *.cm[ixoa] *.cmxa *.[oa]

-include depend
