(* Example for the paper: AM Radio modulation *)

(* Evaluate the following if working at the top level of OCaml

#directory "../../lib";;
#directory "../../lib/backends/Trx";;
#directory "../../lib/backends/C";;
#load "stream.cma";;
(* #load "stream_ocaml.cma";; *)
*)


(* Let's start with the MetaOCaml backend, to generate OCaml code,
  for easier debugging. We switch to C later
*)
module C = Backends.MetaOCaml
module F32 = C.F32

(* Open the strymonas library: higher-level interface, which see
   ../../ocaml/lib/stream_cooked.mli
*)
open Stream_cooked_fn.Make(C)

(* Left-to-right function composition *)
let (>>) f g = fun x -> f x |> g

(* The identity function that makes CPS convenient: see lelt examples below *)
let (let-) c k = c k

let tau = 2. *. Float.pi

(* For carrier wave and other reasons, we need a sine wave
   It is an infinite sine wave stream
   srate is the sampling rate
*)
let sine_wave (srate:int) (freq:float) : F32.t cstream =
  iota (C.int 0) |> 
  map F32.(fun i -> of_int i /. lit (Float.of_int srate)) |>
  map F32.(( *. ) (lit Stdlib.(tau *. freq))) |>
  map F32.sin.invoke

(* Useful for debugging and testing: saving samples to the given,
   pre-allocated array
 *)
let set_array : 'a C.arr -> 'a cstream -> unit C.stm = fun arr str ->
  Raw.zip_raw str (from_to C.(int 0) C.(array_len arr - int 1)) |>
  Raw.iter (fun (v,i) -> C.array_set arr i v)

(* Build an array of the sine wave, for future use and testing *)
(* A of the first octave *)

let bb_arr = 
  let arr = Array.make 4000 (C.F32.to_t 0.) in
  let c = C.one_arg_fun @@ fun arr -> sine_wave 4_000 440. |> 
           set_array arr
  in
  Runcode.run c arr; arr


(* carrier signal *)
(* Medium-wave: 531kHz - 1602 KHz,  10KHz spacing *)

let srate_c = 3360_000 (* samples/sec, twice the medium-wave freq *)

(* AM 540 (kHz) *)
let carrier : F32.t cstream = sine_wave srate_c 540_000.


(* baseband/message signal *)
(* We take it from an array, which may be an mmaped array 
   Assume the array contains F32.t samples, of unit amplitude
*)
let srate_m = 48_000   (* samples/sec: DVD/HDMI rate *)

let message : F32.t C.arr -> F32.t cstream = of_arr

(* The carrier and the message are sampled at different rates. Before
   attempting modulation, we have to bring them to the same rate: upsample
   the message signal.
   For the sake of example we use the simplest left-neighbor upsampling.
   More useful linear/cubic upsampling is obtained by filtering, which
   we introduce only later.
*)

let upsample (rate_from:int) (rate_to:int) : 'a stream -> 'a stream =
    let expansion = rate_to / rate_from in
    assert (expansion > 1 && expansion * rate_from = rate_to );
    flat_map (fun s -> from_to C.(int 0) (C.int (expansion - 1)) |>
                       map (Fun.const s))

(* Amplitude modulation *)
(* 
    y ( t ) = [ 1 + m ( t ) / A ] c ( t ) = [ 1 + mu cos ⁡ ( 2 π f_m t + ϕ ) ] A
    sin ⁡ ( 2 π f_c t ) 
mu <= 1: modulation index
*)

let am_modulate (ms:F32.t cstream) (srate_m:int) 
                (cs:F32.t cstream) (srate_c:int) 
                (mu:float) : F32.t cstream =
  zip_with (fun m c -> F32.((lit 1. +. lit mu *. m) *. c))
        (upsample srate_m srate_c ms) cs

(* Finally, we write the result out: signed 16-bit little-endian integer
   For that, we use a foreign-function procedure
 *)

let write_s16_le : (F32.t C.exp -> unit C.stm) C.ff =
  let wrs16 x = Codelib.letl x @@ fun x -> .<
    let s16 = if .~x > 0x7fff. then 0x7fff else 
              if .~x < -0x7fff. then -0x7fff 
              else int_of_float .~x
    in print_char (s16 land 0xff |> Char.chr);
       print_char ((Int.shift_right s16 8) land 0xff |> Char.chr)>.
  in
  let ff = Trx_code.make_ff wrs16 in
  {invoke = C.dyn >> ff.invoke >> C.inj_stm }

(* Testing the output: play the stream, at the given sample rate *)
let play : int -> F32.t cstream -> int = fun srate st ->
  let c = st |> iter write_s16_le.invoke in
  let (fn,ch) = Filename.open_temp_file "wav" ".ml" in
  let ppf = Format.formatter_of_out_channel ch in
  Codelib.format_code ppf (c |> C.to_code |> close_code); 
  Format.fprintf ppf "%!";
  close_out ch;
  Printf.kprintf Sys.command "ocaml %s | aplay -f S16_LE -r%d" 
    fn srate

(* Play the sine wave, for testing 
let _ =
  sine_wave srate_m 440. |> 
  map F32.( ( *. ) (lit 30000.)) |>
  take C.(int 100000) |> play srate_m
*)

(* put it all together *)

let ammod =
  C.one_arg_fun @@ fun arr ->
  am_modulate (message arr) srate_m carrier srate_c 0.9 |>
  map F32.( ( *. ) (lit 30000.)) |>
  iter write_s16_le.invoke

(*
          val ammod : (float array -> unit) code = .<
  fun arg1_8 ->
    let v_9 = Stdlib.ref 0 in
    for i_10 = 0 to (Stdlib.Array.length arg1_8) - 1 do
      let el_11 = Stdlib.Array.get arg1_8 i_10 in
      for i_12 = 0 to 70 - 1 do
        let t_13 = ! v_9 in
        Stdlib.incr v_9;
        (let t_14 = (Stdlib.Float.of_int t_13) /. 3360000. in
         let t_15 = 3392920.0658769766 *. t_14 in
         let t_16 = Stdlib.sin t_15 in
         let t_17 =
           30000. *. ((1. +. (0.90000000000000002 *. el_11)) *. t_16) in
         let s16_18 =
           if t_17 > 0x7fff.
           then 32767
           else
             if t_17 < (-0x7fff.) then (-32767) else Stdlib.int_of_float t_17 in
         Stdlib.print_char (Stdlib.Char.chr (s16_18 land 255));
         Stdlib.print_char
           (Stdlib.Char.chr ((Stdlib.Int.shift_right s16_18 8) land 255)))
      done
    done>.
*)

(* AM demodulation, from F32.t samples *)

(* Sort of opposite of upsample *)
let decimate : int -> 'a stream -> 'a stream =
  fun n st ->
  assert (n > 1);
  let skip = n - 1 in
  let- z = Raw.initializing_ref C.(int skip) in
  st |> Raw.map_raw ~linear:false 
      C.(fun e k -> if_ (dref z <= int 0) ((z := int skip) @. k e) (decr z))

(* check that decimate is the left unit of upsample *)

let _ =
  let s1 = sine_wave 4_000 440. in
  let s2 = s1 |> upsample 4_000 (16*4_000) |> decimate 16 in
  zip_with F32.( -. ) s1 s2 |>
  take C.(int 10000) |>
  map F32.(fun x -> C.cond (x < lit 0.) (neg x) x) |>
  fold F32.(fun a x -> C.cond (x > a) x a) F32.(lit 0.) |>
  C.run


let demodulate (decimate_by:int) : F32.t cstream -> F32.t cstream =
  map F32.(fun x -> C.cond (x > lit 0.) x (lit 0.)) >> 
  decimate decimate_by

(* Test modulation/demodulation *)
(*
let _ =
  am_modulate (sine_wave srate_m 440.) srate_m carrier srate_c 0.9 |>
  map F32.( ( *. ) (lit 30000.)) |>
  demodulate (srate_c / srate_m) |>
  map F32.( ( -. ) (lit 15000.)) |>
  map F32.( ( *. ) (lit 2.)) |>
  take C.(int 100000) |> play srate_m
*)



(* Better sine wave generation *)
(*
  y = sin(2pi f t) = sin (2pi f (i/srate))
  Can use a faster sin whose argument is guaranteed to be within 0..tau
*)
let sine_wave (srate:int) (freq:float) : F32.t cstream =
  (* phase per sample *)
  let delta = (tau *. freq) /. float_of_int srate in
  Raw.infinite (fun k -> F32.lit delta |> k) |>
  map_accum F32.(fun acc' s k -> 
    let- acc = C.letl (acc' +. s) in
    let- acc = C.letl (C.cond (acc < lit tau) acc (acc -. lit tau)) in
    k acc acc') (F32.lit 0.) |> 
  map F32.sin.invoke


let bb_arr' = 
  let arr = Array.make 4000 (C.F32.to_t 0.) in
  let c = C.one_arg_fun @@ fun arr -> sine_wave 4_000 440. |> 
           set_array arr
  in
  Runcode.run c arr; arr

(* make sure the difference is small *)
let _ = 
  assert (Array.map2 (-.) bb_arr bb_arr' |> 
          Array.fold_left (fun a x -> max a (Float.abs x)) 0.
          < 1e-12)


let carrier : F32.t cstream = sine_wave srate_c 540_000.

let ammod =
  C.one_arg_fun @@ fun arr ->
  am_modulate (message arr) srate_m carrier srate_c 0.9 |>
  map F32.( ( *. ) (lit 30000.)) |>
  iter write_s16_le.invoke

(*
          val ammod : (float array -> unit) code = .<
  fun arg1_42 ->
    let v_43 = Stdlib.ref 0. in
    for i_44 = 0 to (Stdlib.Array.length arg1_42) - 1 do
      let el_45 = Stdlib.Array.get arg1_42 i_44 in
      for i_46 = 0 to 70 - 1 do
        let t_47 = ! v_43 in
        let t_48 = t_47 +. 1.0097976386538621 in
        let t_49 =
          if t_48 < 6.2831853071795862
          then t_48
          else t_48 -. 6.2831853071795862 in
        v_43 := t_49;
        (let t_50 = Stdlib.sin t_47 in
         let t_51 =
           30000. *. ((1. +. (0.90000000000000002 *. el_45)) *. t_50) in
         let s16_52 =
           if t_51 > 0x7fff.
           then 32767
           else
             if t_51 < (-0x7fff.) then (-32767) else Stdlib.int_of_float t_51 in
         Stdlib.print_char (Stdlib.Char.chr (s16_52 land 255));
         Stdlib.print_char
           (Stdlib.Char.chr ((Stdlib.Int.shift_right s16_52 8) land 255)))
      done
    done>.
*)

(* Now, generate C *)
(* C backend *)
module C = Pk_cde.Make(C_cde)
module F32 = C.F32

(* Open the strymonas library: higher-level interface, which see
   ../../ocaml/lib/stream_cooked.mli
*)
open Stream_cooked_fn.Make(C)

let sine_wave (srate:int) (freq:float) : F32.t cstream =
  (* phase per sample *)
  let delta = (tau *. freq) /. float_of_int srate in
  Raw.infinite (fun k -> F32.lit delta |> k) |>
  map_accum F32.(fun acc' s k -> 
    let- acc = C.letl (acc' +. s) in
    let- acc = C.letl (C.cond (acc < lit tau) acc (acc -. lit tau)) in
    k acc acc') (F32.lit 0.) |> 
  map F32.sin.invoke

let carrier : F32.t cstream = sine_wave srate_c 540_000.
let message : F32.t C.arr -> F32.t cstream = of_arr

let upsample (rate_from:int) (rate_to:int) : 'a stream -> 'a stream =
    let expansion = rate_to / rate_from in
    assert (expansion > 1 && expansion * rate_from = rate_to );
    flat_map (fun s -> from_to C.(int 0) (C.int (expansion - 1)) |>
                       map (Fun.const s))

let am_modulate (ms:F32.t cstream) (srate_m:int) 
                (cs:F32.t cstream) (srate_c:int) 
                (mu:float) : F32.t cstream =
  zip_with (fun m c -> F32.((lit 1. +. lit mu *. m) *. c))
      (upsample srate_m srate_c ms) cs

(* Finally, we write the result out: signed 16-bit little-endian integer
   For that, we use a foreign-function procedure
 *)

let write_s16_le : (F32.t C.exp -> unit C.stm) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    {invoke = fun (_,e) -> stmt_app (I.OP.name "write_s16_le") [e]}
  in
  {invoke = C.dyn >> ff.invoke >> C.inj_stm }

let ammod =
  C.arg_base C.tint @@ fun n ->
  C.arg_array n F32.tbase @@ fun arr ->
  C.nullary_proc @@
  (am_modulate (message arr) srate_m carrier srate_c 0.9 |>
  map F32.( ( *. ) (lit 30000.)) |>
  iter write_s16_le.invoke)

let _ = C.pp_proc ~name:"ammod" Format.std_formatter ammod

(*
void ammod(int const n_1,float * const a_2){
  float x_3 = 0.;
  for (int i_4 = 0; i_4 < n_1; i_4 += 1){
    float const t_5 = a_2[i_4];
    for (int i_6 = 0; i_6 < 70; i_6 += 1){
      float const t_7 = x_3;
      float const t_8 = t_7 + 1.0097976;
      float const t_9 = (t_8 < 6.2831853 ? t_8 : t_8 - 6.2831853);
      x_3 = t_9;
      float const t_10 = sinf(t_7);
      float const t_11 = 30000. * ((1. + (0.9 * t_5)) * t_10);
      write_s16_le(t_11);
    }
  }
}

*)

;;
