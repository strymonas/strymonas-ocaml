#if defined(BASELINE)
  #if defined(F1)
    #define ADDSUFFRAW(name) #name"BaselineF1"
  #else
    #define ADDSUFFRAW(name) #name"BaselineF2"
  #endif
  #define ADDSUFF(name) ADDSUFFRAW(name)
#else
  #if defined(F1)
    #define ADDSUFFRAW(name) #name"StrymonasF1"
  #else
    #define ADDSUFFRAW(name) #name"StrymonasF2"
  #endif
  #define ADDSUFF(name) ADDSUFFRAW(name)
#endif
#include "benchmarks.h"
#include "utils.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

#ifndef WARMUP
#define WARMUP 5
#endif
#ifndef REPS
#define REPS 20
#endif

#define fmradio_def               1

#define ADDDEF(name) name##_def
#define STRCMP(benchf, bench) (ADDDEF(benchf) == ADDDEF(bench))

#define RUN1() \
  { int64_t i;\
    for (i=0; i<WARMUP; ++i) {\
      BENCHF1 ();\
    }\
    for (i=0; i<REPS; ++i) {\
      double start = time_in_mili();\
      BENCHF1 ();\
      double end = time_in_mili();\
      result_times[i] = (end - start);\
    }}


char bench[41];
double result_times[REPS];

#if defined(BENCHF1)
int main(void)
{
  bench[40]= '\0';

  #if STRCMP(BENCHF1, fmradio)
    strcpy(bench, ADDSUFF(fmradio_));
    RUN1();
  #else
    #error "invalid bench"
  #endif

  printf("%-40s %10.1lf %10.1lf %5.1lf   ms/op\n",\
        bench,\
        mean(result_times, REPS),\
        mean_error(result_times, REPS),\
        standard_deviation(result_times, REPS));
}

#else // for header
int main(void)
{
  printf("%-40s %10s %10s %5s %7s\n", "Benchmark", "Mean", "Mean-Error", "Sdev", "Unit");
}
#endif