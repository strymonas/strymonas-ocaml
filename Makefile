.PHONY: all 
all: check-compiler lib

.PHONY: check-compiler
check-compiler:
	@test $$(opam switch show) = "4.11.1+BER" \
	|| (echo 1>&2 "OCaml must be 4.11.1+BER"; exit 1)

.PHONY: lib
lib: 
	cd lib && $(MAKE)

.PHONY: test
test:
	cd test && $(MAKE)

.PHONY: clean
clean::
	cd lib && $(MAKE) clean


.PHONY: bench
bench:
	cd benchmarks && $(MAKE)

clean::
	cd benchmarks && $(MAKE) clean

