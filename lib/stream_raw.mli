(* Stream lower-level interface *)

type 'a exp         (* Abstract type of code expressions *)
type 'a stm         (* Abstract type of statements *)
type 'a mut         (* Mutable variables *)
type 'a arr         (* Arrays *)
type 'a tbase       (* Base types *)
type 'a stream      (* Here, 'a is not necessarily the code type! *)

(* Producer of values. When evaluated repeatedly, may produce different values.
   This is the primitive of initalizers and loops.
   The received continuation must be invoked exactly once.
   FYI, the continuation can be thought of as a kind of the yield keyword in generators
   from the point of view of the interface (although it is not correct).
*)
type 'a emit = ('a -> unit stm) -> unit stm

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
  | GExp of bool exp                    (* should be cheap to evaluate *)
  | GRef of bool mut                    (* a boolean flag *)

(* Producers *)
(* The index expression of pull array should do let-insertion!
   pull_array len idx: 0 <= idx < len *)
val pull_array : int exp -> (int exp -> 'a emit) -> 'a stream 

(* Initializers: sort-of let-expressions. They introduce stream state.
   They are also a sort of a flat_map
*)
(* The following introduce local stream variables, so to speak,
   both mutable and immutable.
   Nested streams may have to be `closure converted' (in complicated
   zips), therefore, all local variables, in particular,
   variables whose bound expressions depend on the current element of
   the outer stream, have to be declared using initializing... below.
   The bound/initializing expression is evaluated only once, 
   when the stream starts (that is, before the first element is emitted).
*)

  (* Essentially let-insertion. The initializing expression may be stateful *)
val initializing      : 'z exp -> ('z exp -> 'a stream) -> 'a stream
  (* Mutable state with the given initial value *)
val initializing_ref  : 'z exp -> ('z mut -> 'a stream) -> 'a stream
  (* A *non-empty* array with a statically known, and 
     preferably rather small size. It is generally mutable.
    For immutable (parameters), consider initializing_static_arr
 *)
val initializing_arr  : 'z tbase -> 'z exp array -> 
                       ('z arr -> 'a stream) -> 'a stream
  (* A *non-empty* array with a statically known content.
     It should not be mutated *)
val initializing_static_arr  : 'z tbase -> ('b -> 'z exp) -> 'b array -> 
                       ('z arr -> 'a stream) -> 'a stream
  (* An uninitialized array of the given size. The first argument is
     is the type descriptor.
  *)
val initializing_uarr : 'z tbase -> int -> 
                       ('z arr -> 'a stream) -> 'a stream


(* Create an infinite stream: run step in an infinite loop *)
val infinite : 'a emit -> 'a stream

(* Consumer: the inverse of [infinite] *)
val iter : ('a -> unit stm) -> 'a stream -> unit stm


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
val map_raw : ?linear:bool -> ('a -> 'b emit) -> 'a stream -> 'b stream
val map_raw' : ('a -> 'b) -> 'a stream -> 'b stream

(* Essentially, take_while *)
val guard : goon -> 'a stream -> 'a stream

(* Although filter_raw is a particular form of map_raw, it is worth
   providing on its own: the filter predicates fuse better.
   Also, filter is rather common.
 *)
val filter_raw : ('a -> bool exp) -> 'a stream -> 'a stream

val flat_map_raw : ('a exp -> 'b stream) -> 'a exp stream -> 'b stream

val zip_raw    : 'a stream -> 'b stream -> ('a * 'b) stream
