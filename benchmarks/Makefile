# Benchmarks rely on MetaOCaml since we generate OCaml code
# Benckmarks also rely on MetaOCaml for infrastructure (run OCaml code)
# To benchmark against streaming, batteries, etc. libraries, we obviously
# need to install those libraries
OCAML=metaocaml
OCAMLC=metaocamlc
OCAMLOPT=metaocamlopt
OCAMLDEP=ocamldep
OCAMLFND=ocamlfind -toolchain metaocaml ocamlopt -package streaming,batteries

.SUFFIXES: .ml .mli .cmo .cmi .cmx

COMMON_OBJS=util.cmx benchmark_types.cmx benchmark.cmx benchmark_abstract.cmx
OBJS=$(COMMON_OBJS) benchmark_strymonas.cmx 
OBJS_BASE=$(COMMON_OBJS) benchmark_baseline.cmx 
OBJS_STREAMING=$(COMMON_OBJS) benchmark_streaming.cmx 
OBJS_BATTERIES=$(COMMON_OBJS) benchmark_batteries.cmx 
INCLUDES=-I ../lib -I ../lib/backends/Trx

.PHONY: all clean # intf 
all: lib lib-base lib-streaming lib-batteries

.PHONY: lib lib-base lib-streaming lib-batteries lib-v1-v2 
lib:
	$(OCAMLOPT) -o bench.out $(INCLUDES) lifts.cmx stream.cmxa $(OBJS:.cmx=.ml)

lib-base:
	$(OCAMLOPT) -o bench-base.out $(INCLUDES) lifts.cmx stream.cmxa $(OBJS_BASE:.cmx=.ml)

# External libraries
lib-streaming:
	$(OCAMLFND) -o bench-streaming.out $(INCLUDES) -linkpkg lifts.cmx stream.cmxa $(OBJS_STREAMING:.cmx=.ml)

lib-batteries:
	$(OCAMLFND) -o bench-batteries.out $(INCLUDES) -linkpkg lifts.cmx stream.cmxa $(OBJS_BATTERIES:.cmx=.ml)


.PHONY: bench bench-base bench-streaming bench-batteries bench2
bench: lib
	./bench.out >| bench_result.txt && \
	cat ./bench_result.txt | grep staged   >| bench_staged.dat

# be carefull to ">>" in the first line 
bench-base: lib-base
	./bench-base.out >> bench_result.txt && \
	cat ./bench_result.txt | grep baseline >| bench_baseline.dat

bench-streaming: lib-streaming
	./bench-streaming.out >> bench_result.txt && \
	cat ./bench_result.txt | grep source   >| bench_source.dat

# be carefull to ">>" in the first line 
bench-batteries: lib-batteries
	./bench-batteries.out >> bench_result.txt && \
	cat ./bench_result.txt | grep batteries >| bench_batteries.dat

bench2:
	./bench.out >| bench_v2.txt
	./bench-base.out >| bench_base.txt
	./bench-streaming.out >| bench_streaming.txt

#Generate code for C benchmarks
# C benchmarks are taken and tested in a separate directory:
# ../../C/bench/

.PHONY: benchmark_c
benchmark_c:
	$(OCAML) $(INCLUDES) -I ../lib/backends/C stream.cma benchmark_c.ml

clean:
	rm -f *.cm[ixo] *.[oa] *.txt *.dat *.eps *.out
