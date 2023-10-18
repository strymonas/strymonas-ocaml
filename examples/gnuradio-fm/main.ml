(* for the reference *)
module C = Pk_cde.Make(C_cde)
module F32 = C.F32
module Raw = Stream_raw_fn.Make(C)
open Stream_cooked_fn.Make(C)
open Stream_gnuradio_sdr_fn.Make(C)(Raw)

open Parameters

let ( let- ) c k = c k

(* Main module for benchmark: array input (could be mmmaped array),
   dummy output
*)
let () =
  C.pp_proc ~name:"gr_fmradio" Format.std_formatter @@
  C.arg_base C.tint @@ fun n -> 
  C.arg_array n C32.tbase @@ fun arr ->
  C.nullary_proc @@ begin
    Raw.pull_array C.(array_len arr) C.(array_get arr)
    |> fir_filter_ccf (Fir.lowPassFilter gain samplingRate cutoff trWidth) 
        ~decimation:64
    |> demod_quad_fm ((samplingRate /. 64.) /. (2. *. Float.pi *. maxDev))
    |> iter null_func.invoke
  end

let () =
  C.pp_proc ~name:"gr_fmradio_fread" Format.std_formatter @@
  C.nullary_proc @@ begin
    let open C in
    init_read.invoke () @.
    begin
    file_read_c32 1024
    |> fir_filter_ccf (Fir.lowPassFilter gain samplingRate cutoff trWidth) 
        ~decimation:64
    |> demod_quad_fm ((samplingRate /. 64.) /. (2. *. Float.pi *. maxDev))
    |> iter null_func.invoke
    end
  end


(* Main module for playback *)
let () =
  C.pp_proc ~name:"gr_fmradio" Format.std_formatter @@
  C.arg_base C.tint @@ fun n -> 
  C.arg_array n C32.tbase @@ fun arr ->
  C.nullary_proc @@ begin
    Raw.pull_array C.(array_len arr) C.(array_get arr)
    |> fir_filter_ccf (Fir.lowPassFilter gain samplingRate cutoff trWidth) 
        ~decimation:64
    |> demod_quad_fm ((samplingRate /. 64.) /. (2. *. Float.pi *. maxDev))
    |> iter write_f32_le.invoke
  end

let () =
  C.pp_proc ~name:"gr_fmradio_fread" Format.std_formatter @@
  C.nullary_proc @@ begin
    file_read_ci8_in_c32 1024
    |> fir_filter_ccf (Fir.lowPassFilter gain samplingRate cutoff trWidth) 
        ~decimation:64
    |> demod_quad_fm ((samplingRate /. 64.) /. (2. *. Float.pi *. maxDev))
    |> iter write_f32_le.invoke
  end
