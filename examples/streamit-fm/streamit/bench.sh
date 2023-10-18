#!/bin/sh
set -euo pipefail
# set -euox pipefail

WARMUP="5"
REPS="20" # must be >= 2 due to mean_error

CC="gcc-13"
DBGFLAG="-save-temps"

CFLAG_COMMON="-W -Wall -Wno-tautological-compare -Wno-unused-but-set-variable -DWARMUP=$WARMUP -DREPS=$REPS -pipe"
CFLAGS1="${CFLAG_COMMON} -O3 -march=native -mfpmath=both -fno-math-errno -lm -DF1"
CFLAGS2="${CFLAG_COMMON} -O3 -march=native -mfpmath=both -ffast-math -lm"

OBJS="utils.c benchmarks_gen.c main.c"
OBJSBASE="utils.c benchmarks_base.c main.c"

benchs1=(\
"fmradio")

run() {
  local OPT=$1
  local OUTPUT=$2
  local CFLAGS=$3
  ($CC $CFLAGS -o t.out $OBJS) && ./t.out >| $OUTPUT
  for bench in ${benchs1[@]}; do
    ($CC $CFLAGS -o ${bench}.out -DBENCHF1=$bench $OBJS) && ./${bench}.out >> $OUTPUT
    if [ "$bench" = "fmradio" ]; then
      ($CC $CFLAGS -o ${bench}.out -DBENCHF1=$bench -DBASELINE $OBJSBASE) && ./${bench}.out >> $OUTPUT
    fi
  done
  echo $OUTPUT
}

# ===============================================
echo "The number of repetition is changed by REPS"
run "F1" "result_f1.csv" "$CFLAGS1"
run "F2" "result_f2.csv" "$CFLAGS2"
echo "Completed"
