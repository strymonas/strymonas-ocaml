module type cde_ex     = module type of Cde_ex
module type stream_raw = module type of Stream_raw

module Make(C: cde_ex)(Raw: stream_raw with 
                          type 'a exp = 'a C.exp and
                          type 'a stm = 'a C.stm and
                          type 'a mut = 'a C.mut and
                          type 'a arr = 'a C.arr and
                          type 'a tbase = 'a C.tbase) = struct
type 'a exp   = 'a C.exp 
type 'a stm   = 'a C.stm 
type 'a mut   = 'a C.mut 
type 'a arr   = 'a C.arr 
type 'a tbase = 'a C.tbase
type 'a stream  = 'a Raw.stream
type 'a cstream = 'a exp stream
open Raw

module F32 = C.F32
module Cook = Stream_cooked_fn.Make_ex(C)(Raw)
module Window = Window_fn.Make(C)(Raw)


(* Producers *)
let get_floats : F32.t cstream =
  Cook.iota C.(int 0)
  |> Cook.map F32.of_int


(* Transformers *)
let fir_filter ?(decimation=0) : 
               float array -> F32.t cstream -> F32.t cstream =
  fun weights st ->
    let ntaps = Array.length weights in
    let (module Win) = Window.make_window F32.tbase ntaps (decimation+1) in
    st
    |> Win.make_stream ~padding:false
    |> map_raw F32.(Win.dot tbase lit weights ( +. ) ( *. ))

let monauralize : (F32.t exp * F32.t exp) stream -> F32.t cstream =
  map_raw' F32.(fun (ch1,ch2) -> F32.lit 0.5 *. (ch1 +. ch2))

(** pop = 1, push = 1, peek = 2 *)
let fmDemodulator : float -> float -> float -> F32.t cstream -> F32.t cstream =
  fun sampRate max bandwidth st ->
    let gain = F32.lit (max *. (sampRate /. (bandwidth *. Float.pi))) in
    let (module Win) = Window.make_window F32.tbase 2 1 in
    st
    |> Win.make_stream
    |> map_raw (Win.reduce F32.( *. )) (* mixing of the 2 signals *)
    |> map_raw C.(fun e k -> letl F32.(gain *. atan.invoke e) k)
end
