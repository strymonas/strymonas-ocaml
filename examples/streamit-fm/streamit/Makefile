.PHONY: bench memory clean display_options diff_base
bench: utils.o
	sh bench.sh
	cat result_f1.csv > result_both.csv
	cat result_f2.csv | tail -n +2 >> result_both.csv

memory:
	sh memory.sh

clean:
	rm -rf *.dSYM
	rm -rf .ipynb_checkpoints
	rm *.bc *.i *.s *.out *.o \
	   *.csv *.tmp *.eps

display_options:
	$(CC) $(CFLAGS) -Q --help=warnings

diff_base:
	cd misc && diff -u fmref.c ../benchmarks_base.c >| fmref.patch || :
