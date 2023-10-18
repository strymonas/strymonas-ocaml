#!/bin/sh
set -euo pipefail
# set -euox pipefail

CC="gcc-13"

CFLAG_COMMON="-W -Wall -Wno-tautological-compare -Wno-unused-but-set-variable -pipe -g"
CFLAGS1="${CFLAG_COMMON} -O3 -march=native -mfpmath=both -fno-math-errno -lm"
CFLAGS2="${CFLAG_COMMON} -O3 -march=native -mfpmath=both -ffast-math -lm"

OBJS="benchmarks_gen.c memory_profile.c"
OBJSBASE="benchmarks_base.c memory_profile.c"

benchs1=(\
"fmradio")

run() {
  local OPT=$1
  local CFLAGS=$2
  for bench in ${benchs1[@]}; do
    local SOUTPUT=${bench}_${OPT}_so.out
    ($CC $CFLAGS -o ${SOUTPUT} -DBENCHF1=$bench $OBJS) \
      && valgrind --tool=massif --stacks=yes --massif-out-file=massif.${SOUTPUT} ./${SOUTPUT}
    if [ "$bench" = "fmradio" ]; then
      local BOUTPUT=${bench}_${OPT}_bo.out
      ($CC $CFLAGS -o ${BOUTPUT} -DBENCHF1=$bench -DBASELINE $OBJSBASE) \
        && valgrind --tool=massif --stacks=yes --massif-out-file=massif.${BOUTPUT} ./${BOUTPUT}
    fi
  done
}

# ===============================================
echo "Start"
run "F1"  "$CFLAGS1"
run "F2" "$CFLAGS2"
echo "Completed"
