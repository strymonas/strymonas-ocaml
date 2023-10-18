
/*
The fast (but not very precise, but tolerable) atan2f implementation.
It is based (but not heavily) on GNU Radio's fast_atan2f.cc 
(which is C++, although not very)

https://github.com/gnuradio/gnuradio/blob/b2c9623cbd548bd86250759007b80b61bd4a2a06/gnuradio-runtime/lib/math/fast_atan2f.cc
 * Copyright 2005,2013 Free Software Foundation, Inc.
 *
 * This file is part of GNU Radio
 *
 * SPDX-License-Identifier: GPL-3.0-or-later

The code is greatly re-written, in many cases from scratch
*/

#define PI 3.14159265358979323846
#define PIHALF 1.57079632679489661923

#define TAN_MAP_RES 0.003921569 /* (smallest non-zero value in table) */

/* arctangents from 0 to pi/4  */
static float fast_atan_table[257] = {
    0.000000e+00, 3.921549e-03, 7.842976e-03, 1.176416e-02, 1.568499e-02, 1.960533e-02,
    2.352507e-02, 2.744409e-02, 3.136226e-02, 3.527947e-02, 3.919560e-02, 4.311053e-02,
    4.702413e-02, 5.093629e-02, 5.484690e-02, 5.875582e-02, 6.266295e-02, 6.656816e-02,
    7.047134e-02, 7.437238e-02, 7.827114e-02, 8.216752e-02, 8.606141e-02, 8.995267e-02,
    9.384121e-02, 9.772691e-02, 1.016096e-01, 1.054893e-01, 1.093658e-01, 1.132390e-01,
    1.171087e-01, 1.209750e-01, 1.248376e-01, 1.286965e-01, 1.325515e-01, 1.364026e-01,
    1.402496e-01, 1.440924e-01, 1.479310e-01, 1.517652e-01, 1.555948e-01, 1.594199e-01,
    1.632403e-01, 1.670559e-01, 1.708665e-01, 1.746722e-01, 1.784728e-01, 1.822681e-01,
    1.860582e-01, 1.898428e-01, 1.936220e-01, 1.973956e-01, 2.011634e-01, 2.049255e-01,
    2.086818e-01, 2.124320e-01, 2.161762e-01, 2.199143e-01, 2.236461e-01, 2.273716e-01,
    2.310907e-01, 2.348033e-01, 2.385093e-01, 2.422086e-01, 2.459012e-01, 2.495869e-01,
    2.532658e-01, 2.569376e-01, 2.606024e-01, 2.642600e-01, 2.679104e-01, 2.715535e-01,
    2.751892e-01, 2.788175e-01, 2.824383e-01, 2.860514e-01, 2.896569e-01, 2.932547e-01,
    2.968447e-01, 3.004268e-01, 3.040009e-01, 3.075671e-01, 3.111252e-01, 3.146752e-01,
    3.182170e-01, 3.217506e-01, 3.252758e-01, 3.287927e-01, 3.323012e-01, 3.358012e-01,
    3.392926e-01, 3.427755e-01, 3.462497e-01, 3.497153e-01, 3.531721e-01, 3.566201e-01,
    3.600593e-01, 3.634896e-01, 3.669110e-01, 3.703234e-01, 3.737268e-01, 3.771211e-01,
    3.805064e-01, 3.838825e-01, 3.872494e-01, 3.906070e-01, 3.939555e-01, 3.972946e-01,
    4.006244e-01, 4.039448e-01, 4.072558e-01, 4.105574e-01, 4.138496e-01, 4.171322e-01,
    4.204054e-01, 4.236689e-01, 4.269229e-01, 4.301673e-01, 4.334021e-01, 4.366272e-01,
    4.398426e-01, 4.430483e-01, 4.462443e-01, 4.494306e-01, 4.526070e-01, 4.557738e-01,
    4.589307e-01, 4.620778e-01, 4.652150e-01, 4.683424e-01, 4.714600e-01, 4.745676e-01,
    4.776654e-01, 4.807532e-01, 4.838312e-01, 4.868992e-01, 4.899573e-01, 4.930055e-01,
    4.960437e-01, 4.990719e-01, 5.020902e-01, 5.050985e-01, 5.080968e-01, 5.110852e-01,
    5.140636e-01, 5.170320e-01, 5.199904e-01, 5.229388e-01, 5.258772e-01, 5.288056e-01,
    5.317241e-01, 5.346325e-01, 5.375310e-01, 5.404195e-01, 5.432980e-01, 5.461666e-01,
    5.490251e-01, 5.518738e-01, 5.547124e-01, 5.575411e-01, 5.603599e-01, 5.631687e-01,
    5.659676e-01, 5.687566e-01, 5.715357e-01, 5.743048e-01, 5.770641e-01, 5.798135e-01,
    5.825531e-01, 5.852828e-01, 5.880026e-01, 5.907126e-01, 5.934128e-01, 5.961032e-01,
    5.987839e-01, 6.014547e-01, 6.041158e-01, 6.067672e-01, 6.094088e-01, 6.120407e-01,
    6.146630e-01, 6.172755e-01, 6.198784e-01, 6.224717e-01, 6.250554e-01, 6.276294e-01,
    6.301939e-01, 6.327488e-01, 6.352942e-01, 6.378301e-01, 6.403565e-01, 6.428734e-01,
    6.453808e-01, 6.478788e-01, 6.503674e-01, 6.528466e-01, 6.553165e-01, 6.577770e-01,
    6.602282e-01, 6.626701e-01, 6.651027e-01, 6.675261e-01, 6.699402e-01, 6.723452e-01,
    6.747409e-01, 6.771276e-01, 6.795051e-01, 6.818735e-01, 6.842328e-01, 6.865831e-01,
    6.889244e-01, 6.912567e-01, 6.935800e-01, 6.958943e-01, 6.981998e-01, 7.004964e-01,
    7.027841e-01, 7.050630e-01, 7.073330e-01, 7.095943e-01, 7.118469e-01, 7.140907e-01,
    7.163258e-01, 7.185523e-01, 7.207701e-01, 7.229794e-01, 7.251800e-01, 7.273721e-01,
    7.295557e-01, 7.317307e-01, 7.338974e-01, 7.360555e-01, 7.382053e-01, 7.403467e-01,
    7.424797e-01, 7.446045e-01, 7.467209e-01, 7.488291e-01, 7.509291e-01, 7.530208e-01,
    7.551044e-01, 7.571798e-01, 7.592472e-01, 7.613064e-01, 7.633576e-01, 7.654008e-01,
    7.674360e-01, 7.694633e-01, 7.714826e-01, 7.734940e-01, 7.754975e-01, 7.774932e-01,
    7.794811e-01, 7.814612e-01, 7.834335e-01, 7.853982e-01, 7.853982e-01
};


/* The interface is deliberately copied from atan2f of the standard
   library: man atan2f, but with a simplification.

   float atan2f(float y, float x);

   Calculate  the  principal value of the arc tangent of
   y/x, using the signs of the two arguments to determine the quadrant of
   the result.
   The return value is in radians and within [-pi,pi]

   The simplification is the assumption of -ffast-math, in particular,
   that the arguments are NORMAL floats (no NaNs, no infinites, and no
   -0.0)

   This function uses table lookup and linear interpolation
*/

static inline float fast_atan2f(float const y, float const x) {
  if (y==0.0f) goto yzero;
  if (x==0.0f) goto xzero;

  /* We hereby assume that neither x nor y are zero */
  
  bool pos_y = true; 
  bool pos_x = true;
  float const y_abs = (y < 0.0f ? (pos_y = false), -y : y);
  float const x_abs = (x < 0.0f ? (pos_x = false), -x : x);

  /* 0 <= z <= 1 */
  bool ylessx = true;
  float const z = 
    (y_abs < x_abs ? y_abs / x_abs : ((ylessx = false), (x_abs / y_abs)));

  /* tan z = z when z is sufficiently small */
  float angle = z;
  if (z > TAN_MAP_RES) {
    /* find index and interpolation value */
    /* XXX use modf? */
    float alpha = z * 255.0f;
    int const index = (int)alpha;
    /*
    assert (index >= 0);
    assert (index <= 255);
    */
    alpha -= (float)index;
    /* linear interpolation */
    angle = fast_atan_table[index];
    angle += (fast_atan_table[index + 1] - angle) * alpha;
  }

  if(!ylessx) angle = PIHALF - angle;

  if(pos_x) /* I of IV quadrant */ 
  { 
    if(pos_y) return angle; else return - angle;
  }
  angle -= PI;   /* III quandrant */
  if(pos_y) /* II quadrant */
    return - angle; 
  else         /* III quadrant */
    return angle;
 
  yzero:
  if (x<0.0f) return PI; else return 0.0f;
 
  xzero: /* assuming y is not zero: already checked */
  if (y<0.0f) return -PIHALF; else return PIHALF;
}

/*
Max abs error 1.3113e-06 detected at x=0.5625, y=-1

Max rel error 3.96193e-06 detected at x=0.625, y=0.0625

Average error 5.17388e-07 over 1089 samples
*/

/* The same as fast_atan2f but faster, and less precise.
    We don't do any interpolation
*/

static inline float faster_atan2f(float const y, float const x) {
  if (y==0.0f) goto yzero;
  if (x==0.0f) goto xzero;

  /* We hereby assume that neither x nor y are zero */
  
  bool pos_y = true; 
  bool pos_x = true;
  float const y_abs = (y < 0.0f ? (pos_y = false), -y : y);
  float const x_abs = (x < 0.0f ? (pos_x = false), -x : x);

  /* 0 <= z <= 1 */
  bool ylessx = true;
  float const z = 
    (y_abs < x_abs ? y_abs / x_abs : ((ylessx = false), (x_abs / y_abs)));

  /* tan z = z when z is sufficiently small */
  float angle = z;
  if (z > TAN_MAP_RES) {
    int const index = (int)(z * 255.0f);
    /*
    assert (index >= 0);
    assert (index <= 255);
    */
    angle = fast_atan_table[index];
  }

  if(!ylessx) angle = PIHALF - angle;

  if(pos_x) /* I of IV quadrant */ 
  { 
    if(pos_y) return angle; else return - angle;
  }
  angle -= PI;   /* III quandrant */
  if(pos_y) /* II quadrant */
    return - angle; 
  else         /* III quadrant */
    return angle;
 
  yzero:
  if (x<0.0f) return PI; else return 0.0f;
 
  xzero: /* assuming y is not zero: already checked */
  if (y<0.0f) return -PIHALF; else return PIHALF;
}

/* Hmm, use a finer table? Like twice the size?

Max abs error 0.00366306 detected at x=-1, y=-0.0625

Max rel error 0.058684 detected at x=1, y=0.0625

Average error 0.00104179 over 1089 samples
*/
