# FM Reception about StreamIt


## Bench
`make bench` generates a C function coreesponding to the following strymonas pipeline to stdout:
```:ocaml
get_floats
|> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps) 
              ~decimation:4
|> fmDemodulator samplingRate maxAmplitude bandwidth
|> fir_filter 
    (Fir.equalizer samplingRate bands eqCutoff eqGain numberOfTaps)
|> take numIters
|> iter F32.(fun e -> out := dref out +. e)
```
You can actually benchmark the newly generated code by replacing the code that had already been generated in [streamit/benchmarks_gen.c](streamit/benchmarks_gen.c). In this benchmark, you compare the generated code with http://groups.csail.mit.edu/cag/streamit/apps/benchmarks/fm/c/fmref.c, which is a hand-written reference C implementation of a StreamIt's FM Radio pipeline.

**Requirements for the bench**:
```
$ cd streamit
$ wget http://groups.csail.mit.edu/cag/streamit/apps/benchmarks/fm/c/fmref.c
$ patch -u fmref.c fmref.patch 
$ mv fmref.c benchmarks_base.c
$ make bench
```


## Test
`make test` (resp. `make test_c`) tests the application correctoness by StreamIt interpreter [sit_emulator.ml](sit_emulator.ml) while invoking [test.ml](test.ml) (resp. [test_c.ml](test_c.ml)).