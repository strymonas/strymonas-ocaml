module C = Pk_cde.Make(C_cde)
module F32 = C.F32
module Raw = Stream_raw_fn.Make(C)
open Stream_cooked_fn.Make(C)
open Stream_streamit_sdr_fn.Make(C)(Raw)

let check_identity_list = 
  Util.check_identity_list ~threshold_ave_rel_err:1e-6
                           ~threshold_max_rel_err:1e-6

let check_list ?name reader exp v =
  (* print_endline msg; *)
  let c = C.run_capture_output exp in
  let rec loop acc = 
    match reader c :: acc with
    | exception End_of_file -> Scanf.Scanning.close_in c; List.rev acc
    | acc -> loop acc 
  in 
  let r = loop [] in
  check_identity_list ?name r v

(* `Scanf.bscanf c "%h\n" Fun.id` doesn't work bacause of the last new line *)
let reader_single c = Scanf.bscanf c "%s\n" (fun e -> Scanf.sscanf e "%.17f" Fun.id)
(* let reader_single c = Scanf.bscanf c "%s\n" (fun e -> Scanf.sscanf e "%h" Fun.id) *)
let check_float_list ?name = check_list ?name reader_single


open Parameters

let numLen = 10_000 (* For the non-native OCaml *)
(* let numLen = 100_000 *) (* XXX stack overflow *)

(* Verify each operator *)
let lp1' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps)
      ~decimation:4
  |> iter F32.print
let lp1 = Sit_experiments.(Sit_emulator.run_filter lp1 (get_floats numLen))
let () = check_float_list ~name:"lp1" lp1' lp1

let dem' =
  get_floats
  |> take C.(int numLen)
  |> fmDemodulator samplingRate maxAmplitude bandwidth
  |> iter F32.print
let dem = Sit_experiments.(Sit_emulator.run_filter dem (get_floats numLen))
let () = check_float_list ~name:"dem" dem' dem

let bp1' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.bandPassFilter samplingRate low high numberOfTaps)
  |> iter F32.print
let bp1 = Sit_experiments.(run_band_pass_filter ~rate:samplingRate ~low ~high
                                                ~taps:numberOfTaps (get_floats numLen))
let () = check_float_list ~name:"bp1" bp1' bp1

(* decreasing num_len: get stack overflow otherwise *)
let numLen = 10_000

let eq' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.equalizer samplingRate bands eqCutoff eqGain numberOfTaps)
  |> iter F32.print
let eq = Sit_experiments.(run_equalizer ~rate:samplingRate ~bands ~cutoffs:eqCutoff
                                         ~gains:eqGain ~taps:numberOfTaps (get_floats numLen))
let () = check_float_list ~name:"eq" eq' eq


(* Verify combined operators *)
let lp1_dem' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps)
      ~decimation:4
  |> fmDemodulator samplingRate maxAmplitude bandwidth
  |> iter F32.print
let lp1_dem = Sit_experiments.(Sit_emulator.run_filter lp1 (get_floats numLen) |> Sit_emulator.run_filter dem)
let () = check_float_list ~name:"lp1_dem" lp1_dem' lp1_dem

let fmradio' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps)
      ~decimation:4
  |> fmDemodulator samplingRate maxAmplitude bandwidth
  |> fir_filter (Fir.equalizer samplingRate bands eqCutoff eqGain numberOfTaps)
  |> iter F32.print
let fmradio = Sit_experiments.(
      Sit_emulator.run_filter lp1 (get_floats numLen)
      |> Sit_emulator.run_filter dem
      |> run_equalizer ~rate:samplingRate ~bands ~cutoffs:eqCutoff
                       ~gains:eqGain ~taps:numberOfTaps
    )
let () = check_float_list ~name:"fmradio" fmradio' fmradio

(* These are using 32-bit floats on C side (sit_experiments is OCaml and
   hence uses 64-bit floats)
=== Check lp1 ===
ave_abs_err: 0.00064825302581439034
ave_rel_err: 0.00000012615939924911
max abs err: 0.0057231 = 9368.08 - 9368.08, where i = 1868
max rel err: 6.62518e-07
=== Check dem ===
ave_abs_err: 9.31369323043873720280
ave_rel_err: 0.00000002760118218441
max abs err: 34.1985 = 3.37496e+08 - 3.37496e+08, where i = 239
max rel err: 1.0133e-07
=== Check bp1 ===
ave_abs_err: 0.00000023432995727028
ave_rel_err: 0.00000010098027081450
max abs err: 1.93291e-06 = 4.28914 - 4.28914, where i = 9190
max rel err: 5.68127e-07
=== Check eq ===
ave_abs_err: 0.00000034109758354081
ave_rel_err: 0.00000010387030184281
max abs err: 2.89345e-06 = 6.11653 - 6.11653, where i = 9310
max rel err: 5.2574e-07
=== Check lp1_dem ===
ave_abs_err: 9.25971163687060538905
ave_rel_err: 0.00000002743621641624
max abs err: 31.5895 = 3.37498e+08 - 3.37498e+08, where i = 65
max rel err: 9.35989e-08
=== Check fmradio ===
ave_abs_err: 0.01671405524958003569
ave_rel_err: 0.00000007563443211207
max abs err: 0.0734369 = 220984 - 220984, where i = 34
max rel err: 3.32318e-07
*)

let () = print_endline "filter test in C: All done"
