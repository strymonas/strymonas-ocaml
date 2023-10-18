#ifndef INCLUDED_UTILS_H
#define INCLUDED_UTILS_H

#ifdef __cplusplus
extern "C" 
{
#endif
#include <time.h>
static inline double time_in_mili(void) {
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return (ts.tv_sec)*1000 + (ts.tv_nsec)/1000000;
}

double mean(double arr[], int n);
double var_raw(double arr[], int n);
double var(double arr[], int n);
double unbiased_var(double arr[], int n);
double standard_deviation(double arr[], int n);
double mean_error(double arr[], int n); // confidence == 95%
#ifdef __cplusplus
}
#endif

#endif