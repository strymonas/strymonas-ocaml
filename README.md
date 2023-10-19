# strymonas-ocaml
Strymonas is a code-generation–based library (embedded DSL) for fast, bulk, single-thread in-memory stream processing. This repository focuses on strymonas for OCaml and BER MetaOCaml, which generates C and OCaml code.

Check https://strymonas.github.io/ for the latest situation of the project.


## Building strymonas
There are several options depending on your purpose. Especially, the easiest way is as follows:
```
$ opam switch create 4.14.1+BER
$ eval $(opam env)
$ make lib
$ make test
```

You can see [lib/0README.dr](lib/0README.dr) before exploring the source code to deepen your understandings of the library design.


## Playing with strymonas
There are many examples (and some of them include benchmarks as mentioned below):
- [examples/TryFirst](examples/TryFirst): literally "TryFirst"!
- [examples/amradio](examples/amradio): an SDR application
- [examples/sliding-window](examples/sliding-window): a base of the following two SDR examples (related to FIR filtering)
- [examples/streamit-fm](examples/streamit-fm): an SDR benchmark and application about [StreamIt](https://groups.csail.mit.edu/cag/streamit/)
- [examples/gnuradio-fm](examples/gnuradio-fm): an SDR benchmark and application about [GNU Radio](https://www.gnuradio.org/), generating only C code, depending on [HackRF One](https://greatscottgadgets.com/hackrf/one/)

## Benchmarking strymonas
Micro-benchmarks are in [benchmarks](benchmarks). See [benchmarks/README.md](benchmarks/README.md) for more details.

There are different types of benchmarks, and they can be found in the following directories (see the relevant README.md files for more details):
- [examples/streamit-fm](examples/streamit-fm)
- [examples/gnuradio-fm](examples/gnuradio-fm)




