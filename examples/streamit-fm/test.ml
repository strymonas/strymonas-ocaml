module C = Backends.MetaOCamlExt
module F32 = C.F32
module Raw = Stream_raw_fn.Make(C)
open Stream_cooked_fn.Make(C)
open Stream_streamit_sdr_fn.Make(C)(Raw)

let check_identity_list = Util.check_identity_list

let collect = fold C.(fun z x -> cons x z) C.(nil ())

open Parameters

let numLen = 10_000 (* For the non-native OCaml *)
(* let numLen = 100_000 *) (* XXX stack overflow *)

(* Synthetic input stream used by StreamIt in its benchmarks *)
let get_floats : F32.t cstream =
  iota C.(int 0) |> map F32.of_int

(* Verify each operator *)
let lp1' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps)
      ~decimation:4
  |> collect
  |> C.run
  |> List.rev
let lp1 = Sit_experiments.(Sit_emulator.run_filter lp1 (get_floats numLen))
let () = check_identity_list ~name:"lp1" lp1' lp1


(* gain = 214859173.174:
   string_of_float limits the significant figures to 12 *)
let dem' =
  get_floats
  |> take C.(int numLen)
  |> fmDemodulator samplingRate maxAmplitude bandwidth
  |> collect
  |> C.run
  |> List.rev
(* gain = 214859173.1740587056... *)
let dem = Sit_experiments.(Sit_emulator.run_filter dem (get_floats numLen))
let () = check_identity_list ~name:"dem" dem' dem

let bp1' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.bandPassFilter samplingRate low high numberOfTaps)
  |> collect
  |> C.run
  |> List.rev
let bp1 = Sit_experiments.(run_band_pass_filter ~rate:samplingRate ~low ~high
                                                ~taps:numberOfTaps (get_floats numLen))
let () = check_identity_list ~name:"bp1" bp1' bp1


(* decreasing num_len: get stack overflow otherwise *)
let numLen = 10_000
let eq' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.equalizer samplingRate bands eqCutoff eqGain numberOfTaps)
  |> collect
  |> C.run
  |> List.rev
let eq = Sit_experiments.(run_equalizer ~rate:samplingRate ~bands ~cutoffs:eqCutoff
                                         ~gains:eqGain ~taps:numberOfTaps (get_floats numLen))
let () = check_identity_list ~name:"eq" eq' eq


(* Verify combined operators *)
let lp1_dem' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps)
      ~decimation:4
  |> fmDemodulator samplingRate maxAmplitude bandwidth
  |> collect
  |> C.run
  |> List.rev
let lp1_dem = Sit_experiments.(Sit_emulator.run_filter lp1 (get_floats numLen) |> Sit_emulator.run_filter dem)
let () = check_identity_list ~name:"lp1_dem" lp1_dem' lp1_dem

let fmradio' =
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.lowPassFilter samplingRate cutoffFrequency numberOfTaps)
      ~decimation:4
  |> fmDemodulator samplingRate maxAmplitude bandwidth
  |> fir_filter (Fir.equalizer samplingRate bands eqCutoff eqGain numberOfTaps)
  |> collect
  |> C.run
  |> List.rev
let fmradio = Sit_experiments.(
      Sit_emulator.run_filter lp1 (get_floats numLen)
      |> Sit_emulator.run_filter dem
      |> run_equalizer ~rate:samplingRate ~bands ~cutoffs:eqCutoff
                       ~gains:eqGain ~taps:numberOfTaps
    )
let () = check_identity_list ~name:"fmradio" fmradio' fmradio

(*
=== Check lp1 ===
ave_abs_err: 0.00000002404169125911
ave_rel_err: 0.00000000000048102121
max err: 4.81232e-08 = 99925 - 99925 at pt 19986, rel err 4.81593e-13
=== Check dem ===
ave_abs_err: 0.00009222083312799757
ave_rel_err: 0.00000000000027325132
max err: 9.2268e-05 = 3.37468e+08 - 3.37468e+08 at pt 81, rel err 2.73413e-13
=== Check bp1 ===
ave_abs_err: 0.00000000000069858133
ave_rel_err: 0.00000000000003003906
max err: 1.45661e-12 = 46.4401 - 46.4401 at pt 99813, rel err 3.13654e-14
=== Check eq ===
ave_abs_err: 0.00000000000002526018
ave_rel_err: 0.00000000000000771608
max err: 5.50671e-14 = 6.43475 - 6.43475 at pt 9796, rel err 8.55776e-15
=== Check lp1_dem ===
ave_abs_err: 0.00009222032439486730
ave_rel_err: 0.00000000000027324596
max err: 9.2268e-05 = 3.37478e+08 - 3.37478e+08 at pt 13, rel err 2.73404e-13
=== Check fmradio ===
ave_abs_err: 0.00000005868002334261
ave_rel_err: 0.00000000000026553876
max err: 5.89353e-08 = 220985 - 220985 at pt 1408, rel err 2.66694e-13

With the new bandpass filter
=== Check bp1 ===
ave_abs_err: 0.00000000001599116183
ave_rel_err: 0.00000000000068761710
max err: 3.19957e-11 = 46.4777 - 46.4777 at pt 99894, rel err 6.8841e-13

With new equalizer
ave_abs_err: 0.00000000000021023768
ave_rel_err: 0.00000000000006422420
max err: 4.21885e-13 = 6.51332 - 6.51332 at pt 9916, rel err 6.47726e-14
=== Check fmradio ===
ave_abs_err: 0.00000007457992695117
ave_rel_err: 0.00000000000033748898
max err: 7.48259e-08 = 220985 - 220985 at pt 772, rel err 3.38602e-13

*)

let lpfs' =
  let open Fir in
  get_floats
  |> take C.(int numLen)
  |> fir_filter Fir.(
      seq (lowPassFilter samplingRate 138000000. numberOfTaps)
          (lowPassFilter samplingRate 108000000. numberOfTaps))
  |> collect
  |> C.run
  |> List.rev

let lpfs =
  let open Fir in
  get_floats
  |> take C.(int numLen)
  |> fir_filter (Fir.lowPassFilter samplingRate 138000000. numberOfTaps)
  |> fir_filter (Fir.lowPassFilter samplingRate 108000000. numberOfTaps)
  |> collect
  |> C.run
  |> List.rev

let () = check_identity_list ~name:"lpfs" lpfs' lpfs
(*
=== Check lpfs ===
ave_abs_err: 0.00000000000232766359
ave_rel_err: 0.00000000000000045942
max abs err: 2.36469e-11 = 8979.45 - 8979.45, where i = 8916
max rel err: 2.63344e-15
*)

let () = print_endline "filter test in (non-native) OCaml: All done"
