(* Experiments and tests with StreamIt FMRadio *)

(*
#load "sit_emulator.cmo";;
*)
open Sit_emulator

(*
float->float filter LowPassFilter(float rate, float cutoff, int taps, int decimation) {
  float[taps] coeff;
  init {
    int i;
    float m = taps - 1;
    float w = 2 * pi * cutoff / rate;
    for (i = 0; i < taps; i++) {
      if (i - m/2 == 0)
        coeff[i] = w/pi;
      else
        coeff[i] = sin(w*(i-m/2)) / pi / (i-m/2) *
          (0.54 - 0.46 * cos(2*pi*i/m));
    }
  }
  work pop 1+decimation push 1 peek taps {
    float sum = 0;
    for (int i = 0; i < taps; i++)
      sum += peek(i) * coeff[i];
    push(sum);
    for (int i=0; i<decimation; i++)
	pop();
    pop();
  }
}
*)

let low_pass_filter 
    ~(rate:float) ~(cutoff:float) ~(taps:int) ~(decimation:int) :
    (float,float) filter =
  let pi = Float.pi in
  let coeff = Array.make taps 0. in
  let m = float_of_int (taps - 1) in
  let w = 2.0 *. pi *. cutoff /. rate in
  for i = 0 to taps-1 do
    if (2*i = taps -1) then
      coeff.(i) <- w /. pi
    else
      let im2 = float_of_int i -. m /. 2.0 in
      coeff.(i) <- sin(w *. im2) /. pi /. im2 *.
          (0.54 -. 0.46 *. cos(2.0 *. pi *. float_of_int i /. m))
  done;
  {prework = (fun _ -> ());
   work = fun (module M) ->
     let open M in
     let sum = ref 0. in
     for i = 0 to taps-1 do
       sum := !sum +. peek i *. coeff.(i)
     done;
     push !sum;
     for i=0 to decimation-1 do
	ignore (pop())
     done;
     ignore (pop())
  }


let rec iota : int -> int -> int list = fun l b ->
  if l >= b then [] else l :: iota (l+1) b
let rec const : 'a -> int -> 'a list = fun c b ->
  if b = 0 then [] else c :: const c (b-1)
let get_floats : int -> float list = fun b ->
  iota 0 b |> List.map float_of_int

(* inputs to FMradio *)

(* The dummy input used in StreamIt *)
let ones = iota 0 100 |> List.map float_of_int

open Parameters

(* First stage of Fmradio: low-pass filter *)
(*
    add LowPassFilter(samplingRate, cutoffFrequency, numberOfTaps, 4);
*)

let lp1 = low_pass_filter ~rate:samplingRate ~cutoff:cutoffFrequency
    ~taps:numberOfTaps ~decimation:4 

let _ = run_filter lp1 ones

(*
float->float filter FMDemodulator(float sampRate, float max, float bandwidth) {
  float mGain;

  init {
    mGain = max*(sampRate/(bandwidth*pi));
  }

  work push 1 pop 1 peek 2 {
    float temp = 0;
    //may have to switch to complex?
    temp = (float)(peek(0) * peek(1));
    //if using complex, use atan2
    temp = (float)(mGain * atan(temp));

    pop();
    push(temp);
  }
}
*)

let fm_demodulator ~(sampRate:float) ~(max:float) ~(bandwidth:float) :
    (float,float) filter = 
  let mGain = max *. (sampRate /. (bandwidth *.  Float.pi))
  in
  {prework=(fun _ -> ());
   work=fun (module M) ->
     let open M in
     let temp = (peek(0) *. peek(1)) in
     let temp = mGain *. Float.atan temp in
     ignore (pop());
     push temp
 }

(*
    add FMDemodulator(samplingRate, maxAmplitude, bandwidth);
*)

let dem = fm_demodulator ~sampRate:samplingRate ~max:maxAmplitude
    ~bandwidth:bandwidth

let _ =  run_filter lp1 ones |> run_filter dem

(* 
float->float pipeline BandPassFilter (float rate, float low, float high, int taps) {
  add BPFCore(rate, low, high, taps);
  add Subtracter();
}
float->float splitjoin BPFCore (float rate, float low, float high, int taps) {
  split duplicate;
  add LowPassFilter(rate, low, taps, 0);
  add LowPassFilter(rate, high, taps, 0);
  join roundrobin;
}
float->float filter Subtracter {
  work pop 2 push 1 {
    push(peek(1) - peek(0));
    pop(); pop();
  }
}
*)
let subtracter : (float,float) filter = 
  {prework=(fun _ -> ());
   work=fun (module M) ->
     let open M in
     push (peek(1) -. peek(0));
     ignore (pop()); ignore (pop());
  }

let run_band_pass_filter ~(rate:float) ~(low:float) ~(high:float) ~(taps:int) :
    float list -> float list = fun il ->
  il
  |> duplicate_rr [low_pass_filter ~rate:rate ~cutoff:low
                                    ~taps:taps ~decimation:0
                  ;low_pass_filter ~rate:rate ~cutoff:high
                                  ~taps:taps ~decimation:0]
  |> run_filter subtracter

(* 
float->float pipeline Equalizer(float rate, int bands, float[bands] cutoffs,
                                float[bands] gains, int taps) {
  add EqSplit(rate, bands, cutoffs, gains, taps);
  add float->float filter {
    work pop bands-1 push 1 {
      float sum = 0;
      for (int i = 0; i < bands-1; i++)
        sum += pop();
      push(sum);
    }
  };
}
float->float splitjoin EqSplit(float rate, int bands, float[bands] cutoffs,
                               float[bands] gains, int taps) {
  split duplicate;
  for (int i = 1; i < bands; i++)
    add pipeline {
      add BandPassFilter(rate, cutoffs[i-1], cutoffs[i], taps);
      add Amplify(gains[i]);
    };
  join roundrobin;
}

float->float filter Amplify(float k) {
    work pop 1 push 1 { 
        push(pop() * k); 
    }
}
*)
let adder ~(bands:int) : (float,float) filter = 
  {prework=(fun _ -> ());
   work=fun (module M) ->
     let open M in
     let sum = ref 0. in
     for i = 0 to bands-2 do
       sum := !sum +. pop()
     done;
     push !sum;
  }

let amplify ~(k:float) : (float,float) filter = 
  {prework=(fun _ -> ());
   work=fun (module M) ->
     let open M in 
     push (pop() *. k)
  }

let run_equalizer ~(rate:float) ~(bands:int) ~(cutoffs:float list)
                  ~(gains:float list) ~(taps:int) :
    float list -> float list = fun il ->
  assert (List.length cutoffs = bands);
  assert (List.length gains   = bands);
  il
  |> duplicate_rr' (List.init (bands-1) @@ fun i il' ->
      il'
      |> run_band_pass_filter ~rate ~low:(List.nth cutoffs i) ~high:(List.nth cutoffs (i+1))
                              ~taps
      |> run_filter (amplify ~k:(List.nth gains (i+1)))
    )
  |> run_filter (adder ~bands)


(* 
More realistic input
in US, FMRadio is 87-108 MHz; a channel is 200KHz wide
https://en.wikipedia.org/wiki/Frequency_modulation
See the single-tone modulation
 *)

let carrier = 100_100_000.              (* 100.1 MHz *)
let baseband = 440.                     (* Hz, A of the 1 octave *)

let mod_index = 0.01                    (* modulation index: f_delta/baseband *)
(* it seems that StreamIt assumes narrow-band FM: bandwidth is only 1KHz *)

(* Carrier amplitude is 1.0 *)
let single_tone t =
  cos(2.0 *. Float.pi *. carrier *. t +. mod_index *.
               sin (2.0 *. Float.pi *. baseband *. t))

let single_tone_gen n sampling =
  iota 0 n |> List.map (fun i -> sampling *. float_of_int i |> single_tone)

let _ = single_tone_gen 10_000 samplingRate |> 
    run_filter lp1  |> run_filter dem


let _ = print_endline "Sit test: All Done"
;;
