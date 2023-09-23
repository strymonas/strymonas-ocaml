(* More convenient interface for the stream library *)

module Raw : (module type of Stream_raw)
module C   : (module type of Cde_ex)
open C

type 'a stream  = 'a Raw.stream
type 'a cstream = 'a exp stream     (* Abstract type of streams, carrying
                                       base-type values
                                       Raw streams are more general, and
                                       permit collections such as tuples.
                                       Such collections sometimes require
                                       the descriptor argument, see
                                       Pk_coll module
                                     *)

(* Producers *)
val of_arr         : 'a arr -> 'a cstream
  (* non-empty! And should be immutable *)
val of_static_arr  : 'a tbase -> ('b -> 'a exp) -> 'b array -> 'a cstream 
val of_int_array   : int array -> int cstream       (* Specializations, *)
val of_float_array : float array -> F64.t cstream   (* for tests/benchmarks *)
val iota : int exp -> int cstream
val from_to : ?step:int -> int exp -> int exp -> int cstream
(* We don't provide unfold here: use the Raw interface *)


module Desc : (Pk_coll.desc with type 'a exp = 'a exp and
                                 type 'a stm = 'a stm and
                                 type 'a mut = 'a mut)

(* Consumers *)
val fold_ : ('z,_) Desc.desc ->
  ('z -> 'a -> 'z) -> 'z ->  ('z -> 'w stm) -> 'a stream -> 'w stm

(* fold_ may be confusing to new users, who may think that 'z and 'a
   may be of base types  such as int
   (they actually may, but it is rarely useful).
   They could be tuples of base types -- which is useful.
   Anyway, to avoid confusion we offer a sugared, if less general, 
   specialization of the above
 *)
val fold : ('z exp -> 'a exp -> 'z exp) -> 'z exp -> 'a cstream -> 'z stm
val iter : ('a -> unit stm) -> 'a stream -> unit stm

(* Specializations of the above, convenient 
   especially for testing and benchmarks *)
val find_first : ('a exp -> bool exp) -> 'a exp -> 'a cstream -> 'a stm
val sum_int     : int cstream -> int stm
(*
val average_int : int cstream -> F64.t stm
*)
val count :  'a stream  -> int stm


(* Transformers *)
(* Most transformers are for cstream, for the sake of new users
   Use Raw functions with more general types
 *)
val map      : ('a exp -> 'b exp) -> 'a cstream -> 'b cstream
val flat_map : ('a exp -> 'b stream) -> 'a cstream -> 'b stream
val filter   : ('a exp -> bool exp) -> 'a cstream -> 'a cstream
val take     : int exp -> 'a stream -> 'a stream
val map_accum : ('z exp ->  'a exp -> 
                 ('z exp -> 'b exp -> unit stm) -> unit stm) ->
                'z exp -> 'a cstream -> 'b cstream

val drop       : int exp -> 'a stream -> 'a stream
val drop_while : ('a exp -> bool exp) -> 'a cstream -> 'a cstream
val take_while : ('a exp -> bool exp) -> 'a cstream -> 'a cstream

val zip_with : ('a exp -> 'b exp -> 'c exp) ->
               ('a cstream -> 'b cstream -> 'c cstream)

(* inclusive scan: same as scanl1 in Haskell *)
val scan : ('z exp -> 'a exp -> 'z exp) -> 'z exp -> 'a cstream -> 'z cstream

