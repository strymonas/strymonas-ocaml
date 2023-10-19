# FM Reception about GNU Radio 

- [baseline](baseline) is a GNU Radio FM reception bench for x86-64 
- [baseline-pi](baseline-pi) is a GNU Radio FM reception bench for Raspberry Pi Zero

These benchmarks are compared with strymons-generated C codes about the same FM reception bench as follows:
1. First of all, prepare signal file sources for the bench while your PC is connected to HackRF One:
    ```
    e.g.:
    $ hackrf_transfer -r sps3072000_s8_30s.pcm -f 82500000 -s 3072000 -g 30 -l 40 -a 0 -n 92160000
    $ ffmpeg -f s8 -i sps3072000_s8_30s.pcm -f f32le sps3072000_c32_30s.pcm
    $ head --byte=122880000 sps3072000_c32_30s.pcm > sps3072000_c32_5s.pcm
    $ head --byte=73728000 sps3072000_c32_30s.pcm > sps3072000_c32_3s.pcm
    $ head --byte=49152000 sps3072000_c32_30s.pcm > sps3072000_c32_2s.pcm
    $ cp sps3072000_* baseline
    $ cp sps3072000_* baseline-pi
    ```
2. Select the strymons pipelines in [main.ml](main.ml) for use, then comment out all the other pipelines.
3. `make bench` generates the selected FM reception C code to `/tmp/generated.c`, which is included in [bench_main.c](bench_main.c) (and [play_main.c](play_main.c), which is used for playback test). 
4. Adjust the commented-out sources in [bench_main.c](bench_main.c) depending on your purpose.
5. Compile the code by `gcc -Ofast -march=native -W -Wall utils.c bench_main.c`, for example, and invoke `./a.out` to get a strymonas result.
6. In baseline(-pi), adjust the commented-out sources in the related files, then invoke `make` and `./build/gr-fmradio_gnuradio` to get a GNU Radio result.
  
As for the playback test, see the code and comments in [play_main.c](play_main.c) for more details (especially, about pipeing from hackrf_transfer to ffplay or aplay, for example).


## Playback Test on Headless Raspberry Pi
Use PulseAudio.

- Remote Raspberry Pi:
  1. `default-server = <local macine's local IP>` in `~/.config/pulse/client.conf`
  2. `pulseaudio --start`
  3. test `aplay /usr/share/sounds/alsa/Rear_Center.wav`
- Local machine (the macOS case):
  1. `brew services start pulseaudio`


