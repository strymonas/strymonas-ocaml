/* Test of fast atanf */

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <assert.h>

#include "fast_atanf.h"

int main(void) {
  float abserr_x=0, abserr_y=0;
  float maxabs = 0.0f;
  float relerr_x=0, relerr_y=0;
  float maxrel = 0.0f;
  float errsum = 0.0f;
  int   n = 0;

  for(float x=-1.0; x <= 1.0; x += 1./16.)
   for(float y=-1.0; y <= 1.0; y += 1./16.)
   {
    float const exact  = atan2f(y,x);
    // float const apprx  = fast_atan2f(y,x);
    float const apprx  = faster_atan2f(y,x);
    float const abserr = fabs(exact - apprx);
    float const relerr = abserr / (exact + 1e-7);

    if(abserr > maxabs) {
      abserr_x = x;
      abserr_y = y;
      maxabs = abserr;
    }

    if(relerr > maxrel) {
      relerr_x = x;
      relerr_y = y;
      maxrel = relerr;
    }

    errsum += abserr;
    n += 1;
    }

    printf("\nMax abs error %g detected at x=%g, y=%g\n",
           maxabs,abserr_x,abserr_y);
    printf("\nMax rel error %g detected at x=%g, y=%g\n",
           maxrel,relerr_x,relerr_y);
    printf("\nAverage error %g over %d samples",errsum/(float)n, n);
}
