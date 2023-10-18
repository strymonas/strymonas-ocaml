Remote Raspberry Pi:
1. `default-server = <local IP>` in ~/.config/pulse/client.conf
2. `pulseaudio --start`
3. test `aplay /usr/share/sounds/alsa/Rear_Center.wav`

Local machine (the macOS case):
1. `brew services start pulseaudio`

Signal File Generaion:
```
$ hackrf_transfer -r sps3072000_s8_30s.pcm -f 82500000 -s 3072000 -g 30 -l 40 -a 0 -n 92160000
$ ffmpeg -f s8 -i sps3072000_s8_30s.pcm -f f32le sps3072000_c32_30s.pcm
$ head --byte=122880000 sps3072000_c32_30s.pcm > sps3072000_c32_5s.pcm
$ head --byte=73728000 sps3072000_c32_30s.pcm > sps3072000_c32_3s.pcm
$ head --byte=49152000 sps3072000_c32_30s.pcm > sps3072000_c32_2s.pcm
```