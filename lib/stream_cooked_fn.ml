(* More convenient interface for the stream library *)
(* Beware of using genlet! In nested streams, the expression to let-insert
   may be an effectful expression using reference cells. Therefore, it may be
   moved way too far.
 *)
module type cde_ex     = module type of Cde_ex
module type stream     = module type of Stream_cooked
module type stream_raw = module type of Stream_raw

(* See below for a more convenient to use Make functor *)
module Make_ex(C: cde_ex)(Raw: stream_raw with 
                          type 'a cde   = 'a C.cde and
                          type 'a tbase = 'a C.tbase) :
    (stream with module Raw = Raw) = struct

(* Open C only locally; otherwise its && etc. operators interfere 
   Local opens also makes the code generation places explicit

module C = (Trx_code : (cde with type 'a cde = 'a code))
*)
type 'a cde = 'a C.cde 
type 'a tbase = 'a C.tbase
module Raw = Raw
type 'a stream  = 'a Raw.stream
type 'a cstream = 'a cde stream
open Raw


(* Producers *)
let iota : int cde -> int cstream = fun n ->
  initializing_ref n @@ fun z ->
    infinite @@ fun k -> 
      C.(letl (dref z) @@ fun v -> 
        incr z @.
        k v
        )

(* from_to ~step:n a b: from `a` up to and including `b` by `n` step,
                        negative step is used for descending when a > b
*)
let from_to : ?step:int -> int cde -> int cde -> int cstream =
  fun ?(step=1) a b ->
  if step=1 then pull_array C.(b-a) C.(fun e k -> letl (e+a) k) else
  initializing_ref a @@ fun z ->
    guard (GExp (if step >= 0 then C.(dref z <= b) else C.(dref z >= b))) @@ 
      infinite @@ fun k -> 
        C.(letl (dref z) @@ fun v -> 
          seq (if Stdlib.(step = 1) then incr z
               else if Stdlib.(step = -1) then decr z
               else z := dref z + int step)
            (k v))


let of_arr : 'a array cde -> 'a cstream = fun arr ->
  initializing arr @@ fun arr ->
  initializing C.(array_len arr - int 1) @@ fun len ->
  pull_array len (C.array_get arr)

let of_static_arr : 'a tbase -> 'a cde array -> 'a cstream = fun tb arr ->
  let n = Array.length arr in
  assert (n > 0);
  initializing_arr tb arr @@ fun arr ->
  pull_array (C.int (n-1)) (C.array_get arr)

(* Useful for debugging and benchmarking *)
let of_int_array arr   = arr |> Array.map C.int   |> of_static_arr C.tint
let of_float_array arr = arr |> Array.map C.float |> of_static_arr C.tfloat


(* We don't provide unfold: use the Raw interface along the lines of
   iota or of_arr. It would also probably more efficient too.
 *)

(* Consumers *)
module Desc = Pk_coll.Desc(C)

let fold_ : ('z,_) Desc.desc ->
  ('z -> 'a -> 'z) -> 'z ->  ('z -> 'w cde) -> 'a stream -> 'w cde =
  fun desc f z k str ->
    Desc.newref desc z @@ fun s ->
      C.(seq 
           (iter (fun a -> f (Desc.dref desc s) a |> Desc.set desc s) str)
           (Desc.dref desc s |> k))

(* Folding actions must have a type like this
   because we eliminate even beta-redexes. *)
let fold : ('z cde -> 'a cde -> 'z cde) -> 'z cde -> 'a cstream -> 'z cde =
 fun f z str -> fold_ Desc.Single f z Fun.id str
let iter : ('a -> unit cde) -> 'a stream -> unit cde = iter

(* Transformers *)
let map : ('a cde -> 'b cde) -> 'a cstream -> 'b cstream =
   fun f str -> map_raw (fun a -> C.letl (f a)) str

let filter   : ('a cde -> bool cde) -> 'a cstream -> 'a cstream = filter_raw 

let flat_map : ('a cde -> 'b stream) -> 'a cstream -> 'b stream = flat_map_raw

let zip_with : ('a cde -> 'b cde -> 'c cde) ->
               ('a cstream -> 'b cstream -> 'c cstream) =
 fun f str1 str2 -> zip_raw str1 str2 |> map_raw' (fun (x,y) -> f x y)

(* where n>0,
 take n stream === zip_with (fun _ x -> x) (from_to 1 n) stream
*)
let take : int cde -> 'a stream -> 'a stream =
  fun n st ->
  initializing_ref n @@ fun i ->
  zip_raw (infinite C.(fun k -> seq (decr i) (k ())) |> 
           guard C.(GExp (dref i > int 0))) st |>
  map_raw' (fun (_,x) -> x)

let take_while : ('a cde -> bool cde) -> 'a cstream -> 'a cstream =
  fun f st ->
  initializing_ref (C.bool true) @@ fun zr ->
  st
      (* map_raw is allowed not to call its continuation if the stream is
         already finished
       *)
  |> map_raw C.(fun e k -> if_ (f e) (k e) (zr := bool false))
  |> guard C.(GRef zr)

let map_accum : ('z cde ->  'a cde -> 
                 ('z cde -> 'b cde -> unit cde) -> unit cde) ->
                'z cde -> 'a cstream -> 'b cstream =
  fun tr z st ->
    initializing_ref z @@ fun zr ->
    st |>
    map_raw (fun a k -> 
      C.(letl (dref zr) @@ fun z -> tr z a (fun z' b -> seq (zr := z') (k b))))
(* (* This may be enough in Cooked. *)
let map_accum' : ('z cde -> 'a cde -> ('z cde * 'b cde)) -> 'z cde -> 'a cstream -> 'b cstream =
  fun f zero st ->
  st |> map_accum C.(fun z e k -> let (z', v) = f z e in k z' v) zero *)

let drop : int cde -> 'a stream -> 'a stream =
  fun n st ->
  initializing_ref n @@ fun z ->
  st
  |> filter_raw C.(fun _ -> (dref z <= int 0) || seq (decr z) (bool false))

let drop_while : ('a cde -> bool cde) -> 'a cstream -> 'a cstream =
  fun f st ->
  initializing_ref (C.bool false) @@ fun once ->
  st
  |> filter_raw C.(fun e -> dref once || seq (once := not (f e)) (dref once))


(* Misc. *)
(* an operator with a kind of feedback,
   based on the difference equation y_i = f(y_{i-1}, x_i) *)
let scan : ('z cde -> 'a cde -> 'z cde) -> 'z cde -> 'a cstream -> 'z cstream =
  fun f zero st ->
  st |> map_accum C.(fun z e k -> letl (f z e) @@ fun v -> k v v) zero

let find_first : ('a cde -> bool cde) -> 'a cde -> 'a cstream -> 'a cde = 
  fun f init st -> st |> filter f |> take C.(int 1) |> fold C.(fun _ x -> x) init

let sum_int   : int cstream -> int cde     =
  fun st -> st |> fold C.(+) C.(int 0)

let average_int : int cstream -> float cde  = fun st ->
  C.(newref (int 0) @@ fun z ->
     letl (st |> map_raw' (fun e -> seq (incr z) e) |> sum_int) @@ fun sum ->
     float_of_int sum /. float_of_int (dref z))

let count     :  'a stream -> int cde     = fun st ->
  C.(newref (int 0) @@ fun z ->
    seq (iter (fun _ -> incr z) st) @@ dref z)
end                                     (* Of the Make_ex functor *)


module Make(C: cde_ex) = Make_ex(C)(Stream_raw_fn.Make(C))
