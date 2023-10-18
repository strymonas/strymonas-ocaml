/* disable the logging settings in ~/.gnuradio/config.conf */
#include "fmradio.hpp"
#include "utils.h"

#include <cstdio>
#include <cstdlib>
#include <cassert>
gr_complex *load_file(const char* file_name, size_t * arrlen) {
  size_t desired_size = *arrlen;

  FILE *fp = fopen(file_name, "rb");  

  if ( desired_size == 0 ) {
    desired_size = boost::filesystem::file_size(file_name) / sizeof(gr_complex);
 }
 
  assert ( desired_size > 0 );
  gr_complex *arr = 
    (gr_complex *)malloc(sizeof(gr_complex) * desired_size);
  size_t const real_size = fread(arr, sizeof(gr_complex), desired_size, fp);
  if ( *arrlen > 0 )
  {
    if (desired_size != real_size)
      perror("read error"), exit(1);
  }
  else *arrlen = real_size;

  fclose(fp);
  return arr;
}

#define WARMUP 5
#define REPS 20
double results[REPS];
int main () {
// FOR BENCH:
  size_t arr_size = 0;
  gr_complex *arr = load_file("./sps3072000_c32_3s.pcm",&arr_size);
  std::vector<gr_complex> source(arr, arr + arr_size);
  printf("\nTesting on the array of size %lu\n",arr_size);

  fmradio *top_block;
  int i;
  for (i=0; i<WARMUP; ++i) {
    top_block = new fmradio(source);
    // top_block = new fmradio();
    top_block->tb->start();
    top_block->tb->wait();
    delete top_block;
  }
  for (i=0; i<REPS; ++i) {
    top_block = new fmradio(source);
    // top_block = new fmradio();
    double start = time_in_mili();
    top_block->tb->start();
    top_block->tb->wait();
    double end = time_in_mili();
    results[i] = (end - start);
    delete top_block;
  }

  printf("%-40s %10s %10s %5s %7s\n", "Benchmark", "Mean", "Mean-Error", "Sdev", "Unit");
  printf("%-40s %10.1lf %10.1lf %5.1lf   ms\n",\
        "gr-fmradio_gnuradio",\
        mean(results, REPS),\
        mean_error(results, REPS),\
        standard_deviation(results, REPS));

  return 0;
}
