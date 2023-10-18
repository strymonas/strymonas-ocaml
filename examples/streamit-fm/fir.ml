(* Finite Impulse Response filters (FIR)
   For benchmarking and comparsion, we use exactly the filters
   used by StreamIt 
*)

type weights = float array (* represented by coefficients *)


(* ===== basic operators ===== *)
let scale : float -> weights -> weights =
    fun c -> Array.map (fun e -> c *. e)
let scale_down : float -> weights -> weights =
    fun c -> Array.map (fun e -> e /. c)
let norm : weights -> weights = fun w ->
    let sum = Array.fold_left (+.) 0. w in
    Array.map (fun e -> e /. sum) w

(* ===== fusion operators for weights ===== *)
(* horizontal (parallel) fusion *)
let add : weights -> weights -> weights = Array.map2 ( +. ) 
let sub : weights -> weights -> weights = Array.map2 ( -. ) 
let adds : weights list -> weights = Util.reduce add
let subs : weights list -> weights = Util.reduce sub

(* vertical (serial) fusion,
  Compute convolution of the weights coefficients *)
let seq : weights -> weights -> weights = fun w1 w2 ->
  let l1 = Array.length w1 in
  let l2 = Array.length w2 in
  let l3 = l1 + l2 - 1 in
  let new_weights = Array.make l3 0. in
  for i=0 to l3 - 1 do
    for j=0 to l1 - 1 do
      if ((i - j) >= 0 && (i - j) < l2) then
        new_weights.(i) <- new_weights.(i) +. w1.(j) *. w2.(i - j)
    done
  done;
  new_weights

(* ===== weights ===== *)
let filterBank : 'a list -> ('a -> weights) -> weights list =
  fun params weights_gen ->
    List.map weights_gen params

let lowPassFilter : float -> float -> int -> weights = fun rate cutoff taps ->
  let coeff = Array.make taps 0. in
  let m = float taps -. 1. in (* order *)
  let w = 2. *. Float.pi *. cutoff /. rate in
  (* idk well, but this branching is required in
  ** realFIRFilter of the OFDM implementation *)
  if cutoff = 0. then
    for i = 0 to taps-1 do
      coeff.(i) <- (0.54 -. 0.46 *. cos (2. *. Float.pi 
                                           *. float i 
                                           /. m)) (* hamming window *)
    done
  else
    for i = 0 to taps-1 do
      let t = float i -. m /. 2. in
      if (t = 0.) then
            (* XXX why does not `* (0.54 -. 0.46 ...)`?
            ** https://github.com/gnuradio/gnuradio/blob/b2c9623cbd548bd86250759007b80b61bd4a2a06/gr-filter/lib/firdes.cc#L98 *)
            coeff.(i) <- w /. Float.pi
      else
            coeff.(i) <- sin (w *. t)
                          /. Float.pi
                          /. t
                          *. (0.54 -. 0.46 *. cos (2. *. Float.pi 
                                                      *. float i 
                                                      /. m)) (* hamming window *)
    done;
  coeff

(* Since convolutional filters are linear, 
    filter coeff1 signal - filter coeff2 signal = 
          filter (coeff1-coeff2) signal)
*)
let bandPassFilter : float -> float -> float -> int -> weights =
  fun rate cutoff_l cutoff_h taps ->
    let lph = lowPassFilter rate cutoff_h taps in
    let lpl = lowPassFilter rate cutoff_l taps in
    sub lph lpl

(* Equalizer
    The idea is to split the signal into N bands and amplify by
    a band-specific gain and then re-combine.
    In Math:
    sum_i{ gain_i * filter bandpass_i signal}
    Because convolutional filter is linear:
    a*filter c1 signal + b*filter c2 signal = filter (a*c1 + b*c2) signal
    we can perform the equalization on filter coefficients instead
*)
let equalizer : float -> int -> float list -> float list -> int -> weights =
  fun rate bandsNum cutoffs eqGains taps -> (* XXX bandsNum??? *)
  assert (List.length cutoffs = List.length eqGains);
  (* (cutoffs[0],cutoffs[1]), (cutoffs[1],cutoffs[2]), ... *)
  let bands = List.combine (Util.drop_last cutoffs) (List.tl cutoffs) in
  let params = List.combine (List.tl eqGains) bands in
  filterBank params (fun (g, (pl,ph)) ->
    bandPassFilter rate pl ph taps |> scale g)
  |> adds

(* ===== for debugging ===== *)
let print : weights -> unit =
  Array.iter (fun e -> print_float e; print_newline ())
