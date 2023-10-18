(* Parameters of FM Radio and StreamIt benchmark, taken from StreamIt code *)

(* In US, FMRadio is 87-108 MHz; a channel is 200KHz wide
   https://en.wikipedia.org/wiki/Frequency_modulation
*)

let samplingRate = 250_000_000.         (* 250 MHz *)
let cutoffFrequency = 108_000_000.      (* 108 MHz *)
(* XXX https://www.mathworks.com/help/signal/ref/fir1.html#bulla52-n:
   "The order must be even because odd-order symmetric FIR filters must have zero gain at the Nyquist frequency."
   i.e. taps must be odd as gnuradio in https://github.com/gnuradio/gnuradio/blob/b2c9623cbd548bd86250759007b80b61bd4a2a06/gr-filter/lib/firdes.cc#L710. *)
let numberOfTaps = 64

let maxAmplitude = 27_000.
let bandwidth    = 10_000.              (* 10 KHz *)

(* Equalization, after demodulation *)
let bands = 11
let low  = 55.
let high = 1760.

let eqCutoff = 
  List.init bands (fun i ->
    exp (float i *. (log high -. log low) /. float (bands - 1)
         +. log low)
  )

let eqGain = 
  List.init bands (fun i ->
    if i=0 then 0.
    else
      let t = (float (i - 1) -. (float (bands - 2) /. 2.)) /. 5. in
      if t > 0. then 2. -. t else 2. +. t
  )

