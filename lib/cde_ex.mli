(* Abstract interface for code generation 

   Extended interface that supports user-supplied mapping, etc. actions
*)

include module type of Cde

val ( @. )  : unit cde -> 'a cde -> 'a cde

(* A constant of a base type: 0 or its equivalent *)
val tbase_zero : 'a tbase -> 'a cde

(* Integers *)
val imax   : int cde -> int cde -> int cde
val ( * )  : int cde -> int cde -> int cde
val ( / )  : int cde -> int cde -> int cde
val ( ~-)  : int cde -> int cde
val logand : int cde -> int cde -> int cde

val tfloat : float tbase

(* Floating points *)
val float    : float -> float cde
val ( +. )   : float cde -> float cde -> float cde
val ( -. )   : float cde -> float cde -> float cde
val ( *. )   : float cde -> float cde -> float cde
val ( /. )   : float cde -> float cde -> float cde
val truncate : float cde -> int cde
val atan     : float cde -> float cde
val float_of_int : int cde -> float cde

(* Simple i/o, useful for debugging. Newline at the end *)
val print_int   : int cde   -> unit cde
val print_float : float cde -> unit cde

