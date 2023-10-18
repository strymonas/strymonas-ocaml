#include "fmradio.hpp"

using namespace gr;

fmradio::fmradio (const std::vector<gr_complex>& source)  {
  this->tb = make_top_block("FM Radio", false);

// Blocks:
  this->blocks_vector_source_0 = blocks::vector_source<gr_complex>::make(source);
  this->low_pass_filter_0 = filter::fir_filter_ccf::make(
      decimation,
      filter::firdes::low_pass(
          gain,
          samp_rate,
          cutoff,
          tr_width,
          fft::window::win_type::WIN_HAMMING,
          6.76
          )
      );
  this->fm_demodulator_0 = analog::quadrature_demod_cf::make(
    (samp_rate2/(2*M_PI*max_dev))
  );
  this->null_sink_0 = blocks::null_sink::make(4);
  // this->audio_sink_0 = audio::sink::make(48000);

// Connections:
  this->tb->hier_block2::connect(this->blocks_vector_source_0, 0, this->low_pass_filter_0, 0);
  this->tb->hier_block2::connect(this->low_pass_filter_0, 0, this->fm_demodulator_0, 0);
  this->tb->hier_block2::connect(this->fm_demodulator_0, 0, this->null_sink_0, 0);
  // this->tb->hier_block2::connect(this->fm_demodulator_0, 0, this->audio_sink_0, 0);
}

fmradio::fmradio ()  {
  this->tb = make_top_block("FM Radio", false);

// Blocks:
  this->blocks_file_source_0 = blocks::file_source::make(sizeof(gr_complex), "./sps3072000_c32_30s.pcm", false, 0, 0);
  this->low_pass_filter_0 = filter::fir_filter_ccf::make(
      decimation,
      filter::firdes::low_pass(
          gain,
          samp_rate,
          cutoff,
          tr_width,
          fft::window::win_type::WIN_HAMMING,
          6.76
          )
      );
  this->fm_demodulator_0 = analog::quadrature_demod_cf::make(
    (samp_rate2/(2*M_PI*max_dev))
  );
  this->null_sink_0 = blocks::null_sink::make(4);
  // this->audio_sink_0 = audio::sink::make(48000);

// Connections:
  this->tb->hier_block2::connect(this->blocks_file_source_0, 0, this->low_pass_filter_0, 0);
  this->tb->hier_block2::connect(this->low_pass_filter_0, 0, this->fm_demodulator_0, 0);
  this->tb->hier_block2::connect(this->fm_demodulator_0, 0, this->null_sink_0, 0);
  // this->tb->hier_block2::connect(this->fm_demodulator_0, 0, this->audio_sink_0, 0);
}

fmradio::~fmradio () {
}

