module type cde_ex     = module type of Backends.C
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
module C32 = C.C32
module Cook = Stream_cooked_fn.Make_ex(C)(Raw)
module Window = Window_fn.Make(C)(Raw)

let ( let- ) c k = c k

(* Left-to-right function composition *)
let (>>) f g = fun x -> f x |> g

(* useful FFI functions. Should probably be moved to somewhere else *)
let fast_atan2f : (F32.t C.exp -> F32.t C.exp -> F32.t C.exp) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    F32.{invoke = binary_op (tbase,tbase,I.OP.name "fast_atan2f",tbase)}
  in
  C.{invoke = inj2 ff.invoke}

let faster_atan2f : (F32.t C.exp -> F32.t C.exp -> F32.t C.exp) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    F32.{invoke = binary_op (tbase,tbase,I.OP.name "faster_atan2f",tbase)}
  in
  C.{invoke = inj2 ff.invoke}

let write_s16_le : (F32.t C.exp -> unit C.stm) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    {invoke = fun (_,e) -> stmt_app (I.OP.name "write_s16_le") [e]}
  in
  {invoke = C.dyn >> ff.invoke >> C.inj_stm }

let write_f32_le : (F32.t C.exp -> unit C.stm) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    {invoke = fun (_,e) -> stmt_app (I.OP.name "write_f32_le") [e]}
  in
  {invoke = C.dyn >> ff.invoke >> C.inj_stm }

let null_func : (F32.t C.exp -> unit C.stm) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    {invoke = fun (_,e) -> stmt_app (I.OP.name "null_func") [e]}
  in
  {invoke = C.dyn >> ff.invoke >> C.inj_stm }

let init_read : (unit -> unit C.stm) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    {invoke = fun () -> stmt_app (I.OP.name "init_read") []}
  in
  {invoke = fun () -> C.inj_stm @@ ff.invoke ()}

let read_c32 : (C32.t C.arr -> int C.exp) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    {invoke = fun (_,(_,n),a) -> exp_app tint (I.OP.name "read_c32") [LocalVar a;n]}
  in
  {invoke = (fun (_,arr) -> arr) >> ff.invoke >> C.inj}

let read_ci8_in_c32 : (C32.t C.arr -> int C.exp) C.ff =
  let ff =
    let open C_cde in
    let module I = OffshoringIR in
    {invoke = fun (_,(_,n),a) -> exp_app tint (I.OP.name "read_ci8_in_c32") [LocalVar a;n]}
  in
  {invoke = (fun (_,arr) -> arr) >> ff.invoke >> C.inj}

let file_read_c32 n =
  let dummy_exp = C.int 0 in
  let- buff = Raw.initializing_uarr C32.tbase n in
  let- size = Raw.initializing_ref C.(int 1) in
  Raw.infinite C.(fun k -> (size := read_c32.invoke buff) @. k dummy_exp)
  |> Raw.guard C.(Raw.GExp (dref size > int 0))
  |> Raw.flat_map_raw C.(fun _ ->
      Raw.pull_array (dref size) (array_get buff)
    )

let file_read_ci8_in_c32 n =
  let dummy_exp = C.int 0 in
  let- buff = Raw.initializing_uarr C32.tbase n in
  let- size = Raw.initializing_ref C.(int 1) in
  Raw.infinite C.(fun k -> (size := read_ci8_in_c32.invoke buff) @. k dummy_exp)
  |> Raw.guard C.(Raw.GExp (dref size > int 0))
  |> Raw.flat_map_raw C.(fun _ ->
      Raw.pull_array (dref size) (array_get buff)
    )


(* Transformers *)
(** fir_filter_ABC: A=input, B=output, C=weights *)
let fir_filter_fff ?(decimation=1) : 
               float array -> F32.t cstream -> F32.t cstream =
  fun weights st ->
    let ntaps = Array.length weights in
    let (module Win) = Window.make_window F32.tbase ntaps decimation in
    st
    |> Win.make_stream ~padding:true
    |> map_raw F32.(Win.dot tbase lit weights ( +. ) ( *. ))

let fir_filter_ccf ?(decimation=1) : 
               float array -> C32.t cstream -> C32.t cstream =
  fun weights st ->
    let ntaps = Array.length weights in
    let (module Win) = Window.make_window C32.tbase ntaps decimation in
    st
    |> Win.make_stream ~padding:true
    |> map_raw F32.(Win.dot tbase lit weights C32.( +. ) C32.scale)

let monauralize : (F32.t exp * F32.t exp) stream -> F32.t cstream =
  map_raw' F32.(fun (ch1,ch2) -> F32.lit 0.5 *. (ch1 +. ch2))

(** The Quadrature Demod blocks:
   - https://github.com/gnuradio/gnuradio/blob/master/gr-analog/lib/quadrature_demod_cf_impl.cc#L42
   - https://wiki.gnuradio.org/index.php/Quadrature_Demod

   FM demodulation of quadrature-demodulated signal (I/Q components) *)
let demod_quad_fm (g : float) : C32.t cstream -> F32.t cstream =
  let (module Win) = Window.make_window C32.tbase 2 1 in
  Win.make_stream
  >> map_raw C32.(Win.reduce (fun t0 tprev -> t0 *. conj tprev))
  >> Cook.map F32.(fun e -> 
      lit g *. fast_atan2f.invoke C32.(imag e) C32.(real e))

let demod_quad_fm_faster (g : float) : C32.t cstream -> F32.t cstream =
  let (module Win) = Window.make_window C32.tbase 2 1 in
  Win.make_stream
  >> map_raw C32.(Win.reduce (fun t0 tprev -> t0 *. conj tprev))
  >> Cook.map F32.(fun e -> 
      lit g *. faster_atan2f.invoke C32.(imag e) C32.(real e))
end
