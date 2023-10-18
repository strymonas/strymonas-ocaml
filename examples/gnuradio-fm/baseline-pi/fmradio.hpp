#ifndef FMRADIO_HPP
#define FMRADIO_HPP

#include <gnuradio/top_block.h>
#include <gnuradio/blocks/vector_source.h>
#include <gnuradio/blocks/file_source.h>
#include <gnuradio/filter/firdes.h>
#include <gnuradio/filter/fir_filter_blk.h>
#include <gnuradio/analog/quadrature_demod_cf.h>
#include <gnuradio/blocks/null_sink.h>
// #include <gnuradio/audio/sink.h>



using namespace gr;



class fmradio {

private:
  blocks::vector_source<gr_complex>::sptr blocks_vector_source_0;
  blocks::file_source::sptr blocks_file_source_0;
  filter::fir_filter_ccf::sptr low_pass_filter_0;
  analog::quadrature_demod_cf::sptr fm_demodulator_0;
  blocks::null_sink::sptr null_sink_0;
  // audio::sink::sptr audio_sink_0;

// Variables:
  double gain = 1;
  int samp_rate = 3072000;
  double cutoff = 75000;
  double tr_width = 53 * samp_rate / (22.0 * 65);
  int decimation = 64;
  int samp_rate2 = samp_rate/decimation;
  double max_dev = 75000;

public:
  top_block_sptr tb;
  fmradio(const std::vector<gr_complex>&);
  fmradio();
  ~fmradio();
};


#endif
