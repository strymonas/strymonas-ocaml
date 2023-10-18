/* The main module for playback of Strymonas for GNU Radio
    processing

  The input still comes from the file (should be switched to
  HackRF later, then pipe from the HackRF application, e.g.
  $ hackrf_transfer -r - -f 82500000 -s 3072000 -g 30 -l 40 -a 0
  )
  The output goes on stdout. Pipe to the play application, e.g.
  - $ aplay -t raw -f FLOAT_LE -r48000
  - $ aplay -t raw -f S16_LE -r48000
  - $ ffplay -autoexit -nodisp -f f32le -ar 48000 -ac 1 -i -
  - $ ffplay -autoexit -nodisp -f s16le -ar 48000 -ac 1 -i -
  
  Probably this file should be moved somewhere else...

  We assume that the generated file is "/tmp/generated.c"
  It should use "write_f32_le" for outputing.
  (in the future, consider "write_s16_le": makes the output smaller.
   But one need to set up a proper gain to hear something.)

*/
/*
(Use gcc-13 in macOS)
$ gcc -Ofast -march=native -W -Wall play_main.c
$ ./a.out | ffplay -autoexit -nodisp -f f32le -ar 48000 -ac 1 -i -
$ hackrf_transfer -r - -f 77100000 -s 3072000 -g 30 -l 40 -a 0 | ./a.out | ffplay -autoexit -nodisp -f f32le -ar 48000 -ac 1 -i -

$ gcc -Ofast -mfpu=vfp -mfloat-abi=hard -march=armv6zk -mtune=arm1176jzf-s -W -Wall play_main.c
$ ./a.out | aplay -t raw -f FLOAT_LE -r 48000 -c 1 -i -
$ hackrf_transfer -r - -f 77100000 -s 3072000 -g 30 -l 40 -a 0 | ./a.out | aplay -t raw -f FLOAT_LE -r 48000 -c 1 -i -
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

// prototype of the generated code
void gr_fmradio(int const n_1,float complex * const a_2);
void gr_fmradio_fread(void);

int main(void) {
  size_t arr_size = 0;
  // float complex *arr = load_file("sps3072000_c32_30s.pcm",&arr_size);
  float complex *arr = map_file("sps3072000_c32_30s.pcm",&arr_size);
  fprintf(stderr,"\nTesting on the array of size %lu\n",arr_size);
  // Could do forking here, if really desired. OTH, leave it to shell
  gr_fmradio(arr_size,arr);
  // free(arr); if mapped, needs munpmap. Just leave it

  // global_fp = stdin;
  // gr_fmradio_fread();
}

#include <stdbool.h>
#include <stdint.h>
#include "fast_atanf.h"
#ifndef CMPLXF
#define CMPLXF(x, y) __builtin_complex ((float) (x), (float) (y))
#endif

static inline void init_read(void) {
  fseek(global_fp, 0, SEEK_SET);
}

static inline int read_c32(float complex *arr, int const arr_size) {
  return fread(arr,sizeof(float complex),arr_size,global_fp);
}

static inline int read_ci8_in_c32(float complex *arr, int const arr_size) {
  int8_t buff[arr_size*2];
  int const num = fread(buff, sizeof(int8_t), arr_size*2, global_fp)/2;
  int i;
  for (i = 0; i < arr_size; i += 1)
  {
    arr[i] = CMPLXF((float)buff[i*2] / 127., (float)buff[i*2 + 1] / 127.);
  }

  return num;
}

// write on the stdout
static inline void write_s16_le(float const x) {
  int16_t const y = (int16_t)(x * 127);
  fwrite(&y, sizeof(y), 1, stdout);
}

static inline void write_f32_le(float const x) {
  fwrite(&x,sizeof(x),1,stdout);
}

#include "/tmp/generated.c"

