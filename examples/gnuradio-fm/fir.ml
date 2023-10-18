(* Finite Impulse Response filters (FIR)
   For benchmarking and comparsion, we use exactly the filters
   used by GNU Radio 
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

(* ===== weights ===== *)
(* https://github.com/gnuradio/gnuradio/blob/main/gr-filter/lib/fir_filter.cc#L102
** https://github.com/gnuradio/gnuradio/blob/main/gr-filter/lib/firdes.cc#L77
** Using Float.pi instead of the literal number, as in GNU Radio, doesn't seem
** to have any effect *)
let lowPassFilter : ?is_normalized:bool -> float -> float -> float -> float -> weights =
  fun ?(is_normalized=true) gain freq cutoff transition_width ->
    let taps = 
      let t = int_of_float @@ 53. *. freq /. (22. *. transition_width) in
      if t mod 2 = 0 then t+1 else t
    in
    let m = float taps -. 1. in (* dim *)
    let tau = 2. *. Float.pi in
    let w = tau *. cutoff /. freq in (* normalized angular frequency *)
    let coeff = Array.init taps @@ fun i ->
      let t = float i -. m /. 2. in
      (if (t = 0.) then w /. Float.pi else sin (w *. t) /. (Float.pi *. t))
        *. (0.54 -. 0.46 *. cos (tau *. float i /. m)) (* hamming window *)
    in
    if is_normalized then
      (* https://github.com/gnuradio/gnuradio/blob/main/gr-filter/lib/firdes.cc#L62 *)
      (* XXX reverse for not analitical one? https://github.com/gnuradio/gnuradio/blob/main/gr-filter/lib/fir_filter.cc#L30  *)
      coeff 
      |> scale gain
      |> norm
    else
      coeff 
      |> scale gain

(* ===== for debugging ===== *)
let print : weights -> unit =
  Array.iter (fun e -> print_float e; print_newline ())
