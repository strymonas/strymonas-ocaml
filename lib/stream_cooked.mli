(* More convenient interface for the stream library *)

module Raw : (module type of Stream_raw)

type 'a cde     = 'a Raw.cde        (* Abstract type of code values *)
type 'a tbase   = 'a Raw.tbase      (* Base types *)
type 'a stream  = 'a Raw.stream
type 'a cstream = 'a cde stream     (* Abstract type of streams, carrying
                                       base-type values
                                       Raw streams are more general, and
                                       permit collections such as tuples.
                                       Such collections sometimes require
                                       the descriptor argument, see
                                       Pk_coll module
                                     *)

(* Producers *)
val of_arr         : 'a array cde -> 'a cstream
val of_static_arr  : 'a tbase -> 'a cde array -> 'a cstream (* non-empty! *)
val of_int_array   : int array -> int cstream       (* Specializations, *)
val of_float_array : float array -> float cstream   (* for tests/benchmarks *)
val iota : int cde -> int cstream
val from_to : ?step:int -> int cde -> int cde -> int cstream
(* We don't provide unfold here: use the Raw interface *)
(*
of_channel_in
*)


module Desc : (Pk_coll.desc with type 'a cde = 'a cde)

(* Consumers *)
val fold_ : ('z,_) Desc.desc ->
  ('z -> 'a -> 'z) -> 'z ->  ('z -> 'w cde) -> 'a stream -> 'w cde

(* fold_ may be confusing to new users, who may think that 'z and 'a
   may be of base types  such as int
   (they actually may, but it is rarely useful).
   They could be tuples of base types -- which is useful.
   Anyway, to avoid confusion we offer a sugared, if less general, 
   specialization of the above
 *)
val fold : ('z cde -> 'a cde -> 'z cde) -> 'z cde -> 'a cstream -> 'z cde
val iter : ('a -> unit cde) -> 'a stream -> unit cde

(* Specializations of the above, convenient 
   especially for testing and benchmarks *)
val find_first : ('a cde -> bool cde) -> 'a cde -> 'a cstream -> 'a cde
val sum_int     : int cstream -> int cde
val average_int : int cstream -> float cde
val count :  'a stream  -> int cde


(* Transformers *)
(* Most transformers are for cstream, for the sake of new users
   Use Raw functions with more general types
 *)
val map      : ('a cde -> 'b cde) -> 'a cstream -> 'b cstream
val flat_map : ('a cde -> 'b stream) -> 'a cstream -> 'b stream
val filter   : ('a cde -> bool cde) -> 'a cstream -> 'a cstream
val take     : int cde -> 'a stream -> 'a stream
val map_accum : ('z cde ->  'a cde -> 
                 ('z cde -> 'b cde -> unit cde) -> unit cde) ->
                'z cde -> 'a cstream -> 'b cstream

val drop       : int cde -> 'a stream -> 'a stream
val drop_while : ('a cde -> bool cde) -> 'a cstream -> 'a cstream
val take_while : ('a cde -> bool cde) -> 'a cstream -> 'a cstream

val zip_with : ('a cde -> 'b cde -> 'c cde) ->
               ('a cstream -> 'b cstream -> 'c cstream)

(* inclusive scan: same as scanl1 in Haskell *)
val scan : ('z cde -> 'a cde -> 'z cde) -> 'z cde -> 'a cstream -> 'z cstream

