# strymonas-ocaml
Strymonas is a code-generation–based library (embedded DSL) for fast, bulk, single-thread in-memory stream processing. This repository focuses on strymonas for OCaml and BER MetaOCaml, which generates C and OCaml code.

Check https://strymonas.github.io/ for the latest situation of the project.


## Installing

We offer `strymonas-pure`, a version that does not depend on MetaOCaml, in OPAM:

```bash
$ opam update
$ opam switch create 4.14.1
$ eval $(opam env --switch=4.14.1)
$ opam install strymonas-pure
$ rlwrap ocaml
#require "strymonas-pure";;
module C = Backends_pure.C;;
module C32 = C.C32;;
module F32 = C.F32;;
module Raw = Stream_raw_fn.Make (C);;
module Cook = Stream_cooked_fn.Make_ex (C) (Raw);;
open Cook;;

let pipeline =
  Raw.infinite C32.(fun yield -> yield @@ lit {re=0.; im=7.}) |>
  map F32.(fun e -> C32.imag e *. lit 5.) |>
  filter C.(fun e -> not F32.(equal e (lit 0.))) |>
  take C.(int 5) |>
  map F32.(fun e -> e *. lit 2.) |>
  fold F32.( +. ) F32.(lit 0.);;

let _ = Format.asprintf "%a" (C.pp_proc ~name:"calculate") @@
        C.nullary_proc pipeline;;
# - : string = "\nfloat calculate(){\n  float x_3 = 0.;\n  int x_4 = 5;\n  while (x_4 > 0)\n  {\n    x_4--;\n    x_3 = x_3 + 70.;\n  }\n  return x_3;\n}\n"

let _ = C.run pipeline;;
# - : F32.t = 350.
```

## Building
There are several options depending on your purpose. Especially, the easiest way is as follows:
```
$ opam switch create 4.14.1+BER
$ eval $(opam env)
$ make lib
$ make test
```

You can see [lib/0README.dr](lib/0README.dr) before exploring the source code to deepen your understandings of the library design.


## Example
There are many examples (and some of them include benchmarks as mentioned below):
- [examples/TryFirst](examples/TryFirst): literally, "TryFirst"!
- [examples/sliding-window](examples/sliding-window): a base of [lib/window_fn.ml](lib/window_fn.ml)
- [examples/amradio](examples/amradio): an SDR application
- [examples/streamit-fm](examples/streamit-fm): an SDR benchmark/application related to [StreamIt](https://groups.csail.mit.edu/cag/streamit/)
- [examples/gnuradio-fm](examples/gnuradio-fm): an SDR benchmark/application related to [GNU Radio](https://www.gnuradio.org/) with [HackRF One](https://greatscottgadgets.com/hackrf/one/)

## Benchmarking
Micro-benchmarks are in [benchmarks](benchmarks). See [benchmarks/README.md](benchmarks/README.md) for more details.

There are also macro-benchmarks. They can be found in the following directories (see the relevant README.md files for more details):
- [examples/streamit-fm](examples/streamit-fm)
- [examples/gnuradio-fm](examples/gnuradio-fm)
