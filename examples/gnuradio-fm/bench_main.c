/* The main module for benchmarking Strymonas for GNU Radio
    processing

  Probably this file should be moved somewhere else...

  We assume that the generated file "/tmp/generated.c"
*/
/*
(Use gcc-13 in macOS)
gcc -Ofast -march=native -W -Wall utils.c bench_main.c
gcc -Ofast -mfpu=vfp -mfloat-abi=hard -march=armv6zk -mtune=arm1176jzf-s -W -Wall utils.c bench_main.c
*/
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>
#include <assert.h>
#include <sys/stat.h>

// Read an array of short complex numbers from a given file.
// arrlen on input should point to the desired size, or 0
// In the latter case, we read the whole file, and store the array size
// in that location.
static float complex *load_file(const char* file_name, size_t * arrlen) {
  size_t desired_size = *arrlen;

  FILE *fp = fopen(file_name, "rb");  
  assert( fp != NULL );

  if ( desired_size == 0 ) {
    struct stat stat;
    assert( fstat(fileno(fp),&stat) == 0);
    desired_size = stat.st_size / sizeof(float complex);
 }
 
  assert ( desired_size > 0 );
  float complex *arr = 
    (float complex *)malloc(sizeof(float complex) * desired_size);
  size_t const real_size = fread(arr, sizeof(float complex), desired_size, fp);
  if ( *arrlen > 0 )
  {
    if (desired_size != real_size)
      perror("read error"), exit(1);
  }
  else *arrlen = real_size;

  fclose(fp);
  return arr;
}

#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

// map the file as a short complex array
// Incidentally, if we set MAP_POPULATE as the flag, the behevior is the
// same as load_file
static float complex *map_file(const char* file_name, size_t * arrlen) {
  size_t desired_size = *arrlen;

  int fd = open(file_name, O_RDONLY);  
  assert( fd >= 0 );

  if ( desired_size == 0 ) {
    struct stat stat;
    assert( fstat(fd,&stat) == 0);
    desired_size = stat.st_size / sizeof(float complex);
 }
 
  assert ( desired_size > 0 );
  
  float complex * arr = 
    (float complex *)mmap(NULL,desired_size*sizeof(float complex),
                          PROT_READ,MAP_PRIVATE,fd,0);
  assert (arr != MAP_FAILED);
  close(fd);

  if ( *arrlen == 0 )
    *arrlen = desired_size;
  return arr;
}

static FILE *global_fp;
static void stream_file(const char* file_name) {
  global_fp = fopen(file_name, "rb");
  assert( global_fp != NULL );
}

// prototype of the generated code
void gr_fmradio(int const n_1,float complex * const a_2);
void gr_fmradio_fread(void);

#include "utils.h"
#define WARMUP 5
#define REPS 20
double results[REPS];
int main(void) {
  size_t arr_size = 0;
  // float complex *arr = load_file("sps3072000_c32_30s.pcm",&arr_size);
  float complex *arr = map_file("sps3072000_c32_30s.pcm",&arr_size);
  printf("\nTesting on the array of size %lu\n",arr_size);
  // stream_file("sps3072000_c32_30s.pcm");

  // Take time
  int i;
  printf("%-40s %10s %10s %5s %7s\n", "Benchmark", "Mean", "Mean-Error", "Sdev", "Unit");
  for (i=0; i<WARMUP; ++i) {
    gr_fmradio(arr_size,arr);
    // gr_fmradio_fread();
  }
  for (i=0; i<REPS; ++i) {
    double start = time_in_mili();
    gr_fmradio(arr_size,arr);
    // gr_fmradio_fread();
    double end = time_in_mili();
    results[i] = (end - start);
  }

  // Print time
  printf("%-40s %10.1lf %10.1lf %5.1lf   ms\n",\
         "gr-fmradio_strymonas",\
         mean(results, REPS),\
         mean_error(results, REPS),\
         standard_deviation(results, REPS));

  // free(arr); if mapped, needs munpmap. Just leave it
}

#include <stdbool.h>
#include "fast_atanf.h"

static inline void init_read(void) {
  fseek(global_fp, 0, SEEK_SET);
}

static inline int read_c32(float complex *arr, int const arr_size) {
  return fread(arr,sizeof(float complex),arr_size,global_fp);
}

#include "/tmp/generated.c"

