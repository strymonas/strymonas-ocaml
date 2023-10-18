.PHONY: all 
all: check-compiler lib

.PHONY: check-compiler
check-compiler:
	@test $$(opam switch show) = "4.14.1+BER" \
	|| (echo 1>&2 "OCaml must be 4.14.1+BER"; exit 1)

.PHONY: lib
lib: 
	cd lib && $(MAKE)

.PHONY: clean
clean::
	cd lib && $(MAKE) clean

.PHONY: test
test:
	cd test && $(MAKE) test

.PHONY: bench
bench:
	cd benchmarks && $(MAKE)

clean::
	cd benchmarks && $(MAKE) clean

