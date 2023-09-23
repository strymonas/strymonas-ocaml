(* Abstract interface for code generation 

   Extended interface that supports user-supplied mapping, etc. actions
*)

include module type of Cde

val ( @. )  : unit stm -> 'a stm -> 'a stm
val seqs    : unit stm list -> unit stm

(* A constant of a base type: 0 or its equivalent *)
val tbase_zero : 'a tbase -> 'a exp

(* Integers *)
val imax   : int exp -> int exp -> int exp
val ( * )  : int exp -> int exp -> int exp
val ( / )  : int exp -> int exp -> int exp
val ( ~-)  : int exp -> int exp
val logand : int exp -> int exp -> int exp

(* Simple i/o, useful for debugging. Newline at the end *)
val print_int   : int exp   -> unit stm

(* Foreign function interface *)

(* foreign function type : backends provide the constructors *)
type 'sg ff = private {invoke : 'sg}

(* Numbers of various sorts *)
module type num = sig
  type t
  type num_t
  val tbase  : t tbase
  val to_t   : num_t -> t               (* for the sake of Partial Eval *)
  val of_t   : t -> num_t
  val lit    : num_t -> t exp
  val neg    : t exp -> t exp
  val ( +. ) : t exp -> t exp -> t exp
  val ( -. ) : t exp -> t exp -> t exp
  val ( *. ) : t exp -> t exp -> t exp
  val ( /. ) : t exp -> t exp -> t exp
  val print  : t exp -> unit stm
end

(* Floating point numbers of various sorts *)
module type flonum = sig
  include num
  val rem  : t exp -> t exp -> t exp
  val truncate : t exp -> int exp
  val of_int   : int exp -> t exp
  val sin  : (t exp -> t exp) ff
  val cos  : (t exp -> t exp) ff
  val atan : (t exp -> t exp) ff        (* more can be added *)
end

(* Complex numbers *)
module type cmplxnum = sig
  include num
  type float_t
  val conj     : t exp -> t exp
  val norm2    : t exp -> float_t exp   (* Euclidian norm, squared *)
  val arg      : t exp -> float_t exp   (* -pi to pi *)
  val real     : t exp -> float_t exp
  val imag     : t exp -> float_t exp
  val complex  : float_t exp -> float_t exp -> t exp
  val scale    : float_t exp -> t exp -> t exp
end

module F64 : (flonum with type num_t = float)
module F32 : (flonum with type num_t = float)
module C32 : (cmplxnum with type num_t = Complex.t and type float_t = F32.t)



