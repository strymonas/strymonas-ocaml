#include "utils.h"
#include <math.h>

double mean(double arr[], int n) {
  double result = 0.;
  int i;
  for(i=0; i<n; ++i) {
    result+=arr[i];
  }
  result/=n;
  return result;
}

double var_raw(double arr[], int n) {
  double m = mean(arr, n);

  double result = 0.;
  int i;
  for(i=0; i<n; ++i) {
    const double t = arr[i] - m;
    result += t*t;
  }
  return result;
}

double var(double arr[], int n) {
  return var_raw(arr,n) / n;
}

double unbiased_var(double arr[], int n) {
  return var_raw(arr,n) / sqrt(n-1);
}

double standard_deviation(double arr[], int n) {
  return sqrt(var(arr, n));
}

// (deg_free - 1) |-> t_distribution
double t_distribution_table[30] = {
   12.706,
    4.303,
    3.182,
    2.776,
    2.571,
    2.447,
    2.365,
    2.306,
    2.262,
    2.228,
    2.201,
    2.179,
    2.160,
    2.145,
    2.131,
    2.120,
    2.110,
    2.101,
    2.093,
    2.086,
    2.080,
    2.074,
    2.069,
    2.064,
    2.060,
    2.056,
    2.052,
    2.048,
    2.045,
    2.0
};

// confidence == 95%
double mean_error(double arr[], int n) {
  const double t = t_distribution_table[(n - 1)-1];
  return t * (sqrt(unbiased_var(arr,n))) / (sqrt(n));
}
