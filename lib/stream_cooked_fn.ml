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
                          type 'a exp = 'a C.exp and
                          type 'a stm = 'a C.stm and
                          type 'a mut = 'a C.mut and
                          type 'a arr = 'a C.arr and
                          type 'a tbase = 'a C.tbase) :
    (stream with module Raw = Raw and module C := C) = struct

(* Open C only locally; otherwise its && etc. operators interfere 
   Local opens also makes the code generation places explicit

module C = (Trx_code : (cde with type 'a cde = 'a code))
*)
module C = C
type 'a exp   = 'a C.exp 
type 'a stm   = 'a C.stm 
type 'a mut   = 'a C.mut 
type 'a arr   = 'a C.arr 
type 'a tbase = 'a C.tbase
module Raw = Raw
type 'a stream  = 'a Raw.stream
type 'a cstream = 'a exp stream
open Raw

(* The identity function that makes CPS convenient *)
let (let-) c k = c k

(* Producers *)
let iota : int exp -> int cstream = fun n ->
  initializing_ref n @@ fun z ->
    infinite @@ fun k -> 
      C.(letl (dref z) @@ fun v -> 
        incr z @.
        k v
        )

(* from_to ~step:n a b: from `a` up to and including `b` by `n` step,
                        negative step is used for descending when a > b
*)
let from_to : ?step:int -> int exp -> int exp -> int cstream =
  fun ?(step=1) a b ->
  if step=1 then pull_array C.(b-a + int 1) C.(fun e k -> letl (e+a) k) else
  let- z = initializing_ref a in
  guard (GExp (if step >= 0 then C.(dref z <= b) else C.(dref z >= b))) @@ 
      infinite @@ fun k -> 
        C.(letl (dref z) @@ fun v -> 
          seq (if Stdlib.(step = 1) then incr z
               else if Stdlib.(step = -1) then decr z
               else z := dref z + int step)
            (k v))


let of_arr : 'a arr -> 'a cstream = fun arr ->
  pull_array C.(array_len arr) (C.array_get arr)

let of_static_arr  : 'a tbase -> ('b -> 'a exp) -> 'b array -> 'a cstream =
  fun tb cnv arr ->
  let n = Array.length arr in
  assert (n > 0);
  initializing_static_arr tb cnv arr @@ fun arr ->
  pull_array (C.int n) (C.array_get arr)

(* Useful for debugging and benchmarking *)
let of_int_array arr   = of_static_arr C.tint C.int arr
let of_float_array arr = of_static_arr C.F64.tbase C.F64.lit arr

(* We don't provide unfold: use the Raw interface along the lines of
   iota or of_arr. It would also probably more efficient too.
 *)

(* Consumers *)
module Desc = Pk_coll.Desc(C)

let fold_ : ('z,_) Desc.desc ->
  ('z -> 'a -> 'z) -> 'z ->  ('z -> 'w stm) -> 'a stream -> 'w stm =
  fun desc f z k str ->
    Desc.newref desc z @@ fun s ->
      C.(seq 
           (iter (fun a -> f (Desc.dref desc s) a |> Desc.set desc s) str)
           (Desc.dref desc s |> k))

(* Folding actions must have a type like this
   because we eliminate even beta-redexes. *)
let fold : ('z exp -> 'a exp -> 'z exp) -> 'z exp -> 'a cstream -> 'z stm =
 fun f z str -> fold_ Desc.Single f z C.ret str
let iter : ('a -> unit stm) -> 'a stream -> unit stm = iter

(* Transformers *)
let map : ('a exp -> 'b exp) -> 'a cstream -> 'b cstream =
   fun f str -> map_raw (fun a -> C.letl (f a)) str

let filter   : ('a exp -> bool exp) -> 'a cstream -> 'a cstream = filter_raw 

let flat_map : ('a exp -> 'b stream) -> 'a cstream -> 'b stream = flat_map_raw

let zip_with : ('a exp -> 'b exp -> 'c exp) ->
               ('a cstream -> 'b cstream -> 'c cstream) =
 fun f str1 str2 -> zip_raw str1 str2 |> map_raw' (fun (x,y) -> f x y)

(* where n>0,
 take n stream === zip_with (fun _ x -> x) (from_to 1 n) stream
*)
let take : int exp -> 'a stream -> 'a stream =
  fun n st ->
  initializing_ref n @@ fun i ->
  zip_raw (infinite C.(fun k -> seq (decr i) (k ())) |> 
           guard C.(GExp (dref i > int 0))) st |>
  map_raw' (fun (_,x) -> x)

let take_while : ('a exp -> bool exp) -> 'a cstream -> 'a cstream =
  fun f st ->
  initializing_ref (C.bool true) @@ fun zr ->
  st
      (* map_raw is allowed not to call its continuation if the stream is
         already finished
       *)
  |> map_raw C.(fun e k -> if_ (f e) (k e) (zr := bool false))
  |> guard C.(GRef zr)

let map_accum : ('z exp ->  'a exp -> 
                 ('z exp -> 'b exp -> unit stm) -> unit stm) ->
                'z exp -> 'a cstream -> 'b cstream =
  fun tr z st ->
    initializing_ref z @@ fun zr ->
    st |>
    map_raw (fun a k -> 
      C.(letl (dref zr) @@ fun z -> tr z a (fun z' b -> seq (zr := z') (k b))))
(* (* This may be enough in Cooked. *)
let map_accum' : ('z exp -> 'a exp -> ('z exp * 'b exp)) -> 'z exp -> 'a cstream -> 'b cstream =
  fun f zero st ->
  st |> map_accum C.(fun z e k -> let (z', v) = f z e in k z' v) zero *)

let drop : int exp -> 'a stream -> 'a stream =
  fun n st ->
  initializing_ref n @@ fun z ->
  st
  |> map_raw ~linear:false C.(fun e k -> if_ (dref z <= int 0) (k e) (decr z))

let drop_while : ('a exp -> bool exp) -> 'a cstream -> 'a cstream =
  fun f st ->
  initializing_ref (C.bool false) @@ fun once ->
  st
  |> map_raw ~linear:false C.(fun e k -> 
      if_ (dref once) (k e) (if1 (not (f e)) ((once := bool true) @. k e)))

(* Misc. *)
(* an operator with a kind of feedback,
   based on the difference equation y_i = f(y_{i-1}, x_i) *)
let scan : ('z exp -> 'a exp -> 'z exp) -> 'z exp -> 'a cstream -> 'z cstream =
  fun f zero st ->
  st |> map_accum C.(fun z e k -> letl (f z e) @@ fun v -> k v v) zero

let find_first : ('a exp -> bool exp) -> 'a exp -> 'a cstream -> 'a stm = 
  fun f init st -> st |> filter f |> take C.(int 1) |> fold C.(fun _ x -> x) init

let sum_int   : int cstream -> int stm     =
  fun st -> st |> fold C.(+) C.(int 0)

let average_int : int cstream -> C.F64.t stm  = fun st ->
  let open C in
  let- cnt = newref (int 0) in
  let- sum = newref (int 0) in
  iter (fun e -> incr cnt @. (sum := dref sum + e)) st @.
  ret C.F64.(of_int (dref sum) /. of_int (dref cnt))

(* It's good but fold takes cstream ... 
let count     :  'a stream -> int stm     = fun st ->
  C.(fold (fun z _ -> z + int 1) (int 0) st)
*)

let count     :  'a stream -> int stm     = fun st ->
  C.(fold_ Desc.Single (fun z _ -> z + int 1) (int 0) ret st)

end                                     (* Of the Make_ex functor *)


module Make(C: cde_ex) = Make_ex(C)(Stream_raw_fn.Make(C))
