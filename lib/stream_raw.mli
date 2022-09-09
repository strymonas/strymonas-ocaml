(* Stream lower-level interface *)

type 'a cde         (* Abstract type of code values *)
type 'a tbase       (* Base types *)
type 'a stream      (* Here, 'a is not necessarily the code type! *)

(* Producer of values. When evaluated repeatedly, may produce different values.
   This is the primitive of initalizers and loops.
   The received continuation must be invoked exactly once.
   FYI, the continuation can be thought of as a kind of the yield keyword in generators
   from the point of view of the interface (although it is not correct).
*)
type 'a emit = ('a -> unit cde) -> unit cde

(* The termination condition: true to continue, false to terminate the stream.
   It should be cheap to evaluate.
   It may depend on the stream state (introduced by initalizing_ref etc.
   below), but it must not change state. Furthermore, it must satisfy
   one of the following side-conditions:
   (1) the state used by the termination condition should not be modified
   by map_raw, filter or flat_map operations further down the pipeline.
   The simplest way to ensure that is to make all reference cells used by the
   termination condition private and not available to further operations.
   (for an example, see the implementation of from_to on stream_cooked_fn.ml).
   To say it differently, if map/filter/flatmap need state, it should allocate
   (using initalizing_ref, etc.) private state for itself and not interfere
   in other operations state.
   (2) At times, it is necessary for the termination condition and
   the mapping function to share state and for the mapping to modify it.
   (example: take_while in stream_cooked_fn.ml). In that case, the following 
   conditions must be preserved:
      (a) if the termination condition evaluates to false, no further changes
      in the state should turn it to true.
      (b) if the mapping function changes the state in such a way that
      a termination condition in a guard somewhere (either up or down the
      pipeline) becomes false, the mapping function must not produce
      an stream item (it should `skip': avoid invoking its continuation).
     It is easiest to ensure the preservation of these conditions if we
     allocate a fresh mutable boolean variable, initialized to true.
     If a mapping/filtering function wants to signal the termination,
     it should assign it false (and `skip'). That is, assignments 
     to such mutable varioable should be `monotone': any new value should be
     ANDed with the existing value in this mutable cell.
 *)
type goon = 
  | GTrue                               (* constant true *)
  | GExp of bool cde                    (* should be cheap to evaluate *)
  | GRef of bool ref cde                (* a boolean flag *)

(* Producers *)
(* The index expression of pull array should do let-insertion!
   pull_array upb idx: 0 <= idx <= upb *)
val pull_array : int cde -> (int cde -> 'a emit) -> 'a stream 

(* Initializers: sort-of let-expressions. They introduce stream state.
   They are also a sort of a flat_map
*)
(* The first argument is an initializing expression, which may be
   effectful. It is evaluated only once, when the stream starts (that is,
   before the first element is emitted).
   initializing_ref allocates a reference cell with a given initial expression
*)
val initializing      : 'z cde -> ('z cde -> 'a stream) -> 'a stream
val initializing_ref  : 'z cde -> ('z ref cde -> 'a stream) -> 'a stream
val initializing_uref : 'z cde -> ('z ref cde -> 'a stream) -> 'a stream
val initializing_arr  : 'z tbase -> 'z cde array -> 
                       ('z array cde -> 'a stream) -> 'a stream
val initializing_uarr : 'z tbase -> int -> 
                       ('z array cde -> 'a stream) -> 'a stream


(* Create an infinite stream: run step in an infinite loop *)
val infinite : 'a emit -> 'a stream

(* Consumer: the inverse of [infinite] *)
val iter : ('a -> unit cde) -> 'a stream -> unit cde


(* Transformers *)
(* map_raw is assured to be applied in order. So, it is actually
   an accumulating map_filter.
   It could maintain its own state. It must not, however, change the state
   used by guard.
*)
(* The optional argument, ?linear, tells if the transformer is
   linear -- that is, if the continuation ('b -> unit cde) is invoked
   exactly once. 
   By default it is true.
   If the continuation ends up not being invoked in some cases
   (that is, map_raw behaves like map_option), be sure specify
   ~linear:false!
   On no occasion should continuation be invoked multiple times!
   (because we expect goon to be evaluated once each time
    before calling the continuation)
*)
val map_raw : ?linear:bool ->
  ('a -> ('b -> unit cde) -> unit cde) ->
  'a stream -> 'b stream
val map_raw' : ('a -> 'b) -> 'a stream -> 'b stream

(* Essentially, take_while *)
val guard : goon -> 'a stream -> 'a stream

(* Although filter_raw is a particular form of map_raw, it is worth
   providing on its own: the filter predicates fuse better.
   Also, filter is rather common.
 *)
val filter_raw : ('a -> bool cde) -> 'a stream -> 'a stream

val flat_map_raw : ('a cde -> 'b stream) -> 'a cde stream -> 
  'b stream

val zip_raw    : 'a stream -> 'b stream -> ('a * 'b) stream
