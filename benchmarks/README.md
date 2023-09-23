Benchmarking strymonas

Benchmarks rely on MetaOCaml since we generate OCaml code.
Benckmarks also rely on MetaOCaml for infrastructure: to run OCaml code.
To benchmark against streaming, batteries, etc. libraries, we obviously
need to install those libraries.

`make bench` : run the strymonas benhcmarks. The result is in the file
      bench_result.txt and also bench_staged.dat

`make bench-base` : run the baseline (that is, handwritten code) benchmarks.
      The results are added to bench_result.txt and also separated into
      bench_baseline.dat

For external libraries
`make bench-streaming`
`make bench-batteries`

`make benchmark_c`
Generate code for C benchmarks
C benchmarks are taken and tested in a separate directory: ../../C/bench/

The handwritten code for the benchmarks (the baseline) is in
`benchmark_baseline.ml`

The strymonas benchmarks (and also benchmarks for other libraries) are in
`benchmark_abstract.ml`
