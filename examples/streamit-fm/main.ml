(* the main file for the fmradio bench: run it to generate code
   on the standard output
 *)

 module C = Backends.C
 module F32 = C.F32
 module Raw = Stream_raw_fn.Make(C)
 open Stream_cooked_fn.Make(C)
 open Stream_streamit_sdr_fn.Make(C)(Raw)
 
 open Parameters
 
 let ( let- ) c k = c k
 let numIters = C.int 1_000_000
 
 (* accumulate sum in out *)
 let () =
   C.pp_proc ~name:"fmradio" Format.std_formatter @@
   C.nullary_proc @@
   let open C in
   let- out = newref F32.(lit 0.) in
   begin
   get_floats
   |> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps) 
                 ~decimation:4
   |> fmDemodulator samplingRate maxAmplitude bandwidth
   |> fir_filter 
       (Fir.equalizer samplingRate bands eqCutoff eqGain numberOfTaps)
   |> take numIters
   |> iter F32.(fun e -> out := dref out +. e)
   end @.
   (ret (dref out))
 
 
 