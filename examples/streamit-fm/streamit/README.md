First of all, change CC in bench.sh and memory.sh accodingly.
benchmarks_base.c is based on http://groups.csail.mit.edu/cag/streamit/apps/benchmarks/fm/c/fmref.c.
Apply fmref.patch fot it and rename it to benchmarks_base.c:
```
$ patch -u fmref.c fmref.patch 
$ mv fmref.c benchmarks_base.c
```


`make` and `make bench` invoke the time bench: bench.sh.
`make time` invokes the memory bench: memory.sh.

