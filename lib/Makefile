OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
# Some facilities require MetaOCaml
METAOCAMLC=metaocamlc
METAOCAMLOPT=metaocamlopt

.SUFFIXES: .ml .mli .cmo .cmi .cmx

LIB=stream.cma
LIBOPT=stream.cmxa
# Pure OCaml, not MetaOCaml
LIBPURE=stream_ocaml.cma

# Trx backend needs MetaOCaml; the others are pure OCaml
BACKC=backends/C
BACKENDC=\
  $(BACKC)/c_ast.cmi $(BACKC)/c_pp.cmo $(BACKC)/c_cde.cmo
BACKTRX=backends/Trx
BACKENDS=\
  $(BACKENDC) \
  $(BACKTRX)/trx_code.cmo

# Order matters!
INTF=cde.cmi cde_ex.cmi stream_raw.cmi stream_cooked.cmi 
# OBJS are pure OCaml objects
OBJS=pk_cde.cmo pk_coll.cmo stream_raw_fn.cmo stream_cooked_fn.cmo
OBJSB=$(OBJS) backends.cmo
INCLUDES=-I $(BACKC) -I $(BACKTRX)

.PHONY: all
all: lib opt pure
lib: $(LIB)
opt: $(LIBOPT)
pure: $(LIBPURE)

.PHONY: intf			#Just the interfaces for testing examples
intf: $(INTF)

$(LIB): $(INTF) $(BACKENDS) $(OBJSB)
	$(OCAMLC) -a -o $@ $(filter-out %.cmi,$^)

$(LIBOPT): $(INTF) $(BACKENDS:.cmo=.cmx) $(OBJSB:.cmo=.cmx)
	$(OCAMLOPT) -a -o $@ $(filter-out %.cmi,$^)

$(LIBPURE): $(INTF) $(BACKENDC) $(OBJS)
	$(OCAMLC) -a -o $@ $(filter-out %.cmi,$^)

.mli.cmi:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmo:
	$(OCAMLC) -c $(INCLUDES) $<
.ml.cmx:
	$(OCAMLOPT) -c $(INCLUDES) $<


$(BACKTRX)/trx_code.cmo: $(BACKTRX)/trx_code.ml
	$(METAOCAMLC) -c  $<

$(BACKTRX)/trx_code.cmx: $(BACKTRX)/trx_code.ml
	$(METAOCAMLOPT) -c  $<

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
