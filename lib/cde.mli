(* Abstract interface for code generation 

   Which code generation facilities we really need for this library

   This interface concerns the code generation used in strymonas
   itself. The `semantic actions' (user supplied code for mapping, etc)
   generally uses a richer interface: see cde_ex
*)

type 'a cde                             (* Abstract type of code *)

type 'a tbase                           (* Base types *)
val tbool : bool tbase                  (* More can/will be added later *)
val tint  : int tbase                   (* Backends may provide more 
                                           base types *)

(* Local let, without movement *)
val letl : 'a cde -> (('a cde -> 'w cde) -> 'w cde)
val glet : 'a cde -> 'a cde             (* possibly let-insertion, at some
                                           higher place. That is, movement
                                           is OK
                                         *)

val seq  : unit cde -> 'a cde -> 'a cde 
val unit : unit cde

(* Booleans *)
val bool : bool -> bool cde
val not  : bool cde -> bool cde
val (&&) : bool cde -> bool cde -> bool cde
val (||) : bool cde -> bool cde -> bool cde

(* Integers *)
val int   : int -> int cde
val ( + ) : int cde -> int cde -> int cde
val ( - ) : int cde -> int cde -> int cde
val (mod) : int cde -> int cde -> int cde
val ( =)  : int cde -> int cde -> bool cde
val ( <)  : int cde -> int cde -> bool cde
val ( >)  : int cde -> int cde -> bool cde
val (<=)  : int cde -> int cde -> bool cde
val (>=)  : int cde -> int cde -> bool cde

(* It turns out that Stdlib.min is slow! It was responsible for
   the big slowdown in one of the benchmarks. It seems OCaml does not
   inline min and generates a function call instead.
 *)
val imin : int cde -> int cde -> int cde


(* Control operators *)
(* Separate if_ expression from if-statement *)
val cond : bool cde -> 'a cde -> 'a cde -> 'a cde
val if_  : bool cde -> unit cde -> unit cde -> unit cde
val if1  : bool cde -> unit cde -> unit cde

val for_ : int cde ->           (* exact lower bound *)
           int cde ->           (* exact upper bound *)
           ?guard:bool cde ->   (* possibly a guard, terminate when false *)
           ?step:int cde ->     (* step *)
           (int cde -> unit cde) -> unit cde

val while_ : bool cde -> unit cde -> unit cde

(* Loop with continue: execute the body, which may request to be
   re-executed (like C `continue')
 *)
val cloop :
    ('a -> unit cde) ->        (* cloop's continuation *)
    bool cde option ->         (* possibly a guard. 
                                  It is true at the beginning *)
    (('a -> unit cde) -> unit cde) ->   (* body, in CPS, which may exit
                                           without calling its continuation,
                                           in which case the loop is re-executed
                                           provided the guard is still true.
                                         *)
    unit cde


(* Reference cells *)
(* It makes sense to combine newref with letl, so to immediately bind the
   reference value. This way we may reasonably reproduce `variables' in
   imperative languages. Anyway we always immediately bind a ref cell to
   a variable in our code. 
*)
val newref  : 'a cde -> ('a ref cde -> 'w cde) -> 'w cde
(* Create an *uninitialized* reference cell. The first argument is
   not an initial value: it is just a hint to get the type.
   Therefore, the first argument may be unusable as the value.
   A back end may still use some other value of the same type as
   the initial value.
 *)
val newuref : 'a cde -> ('a ref cde -> 'w cde) -> 'w cde
val dref    :  'a ref cde -> 'a cde
val (:=)    :  'a ref cde -> 'a cde -> unit cde
val incr    : int ref cde -> unit cde
val decr    : int ref cde -> unit cde

(* Arrays. They have to be of base types (although what is a base
   type varies with a backend)
*)
(* XXX Later consider constant arrays, mutable arrays and mutable
   extensible arrays
*)
(* Values of 'a array cde denotes a _value_ (usually, variable name),
   never an expression to create such an array
 *)
val array_get' : 'a array cde -> int cde -> 'a cde
(* It makes sense to combine it with letl *)
val array_get  : 'a array cde -> int cde -> ('a cde -> 'w cde) -> 'w cde
val array_len  : 'a array cde -> int cde
val array_set  : 'a array cde -> int cde -> 'a cde -> unit cde
(* initialized non-empty array, immediately bound to a variable.
   Must be of a base type
 *)
val new_array  : 'a tbase -> 'a cde array -> ('a array cde -> 'w cde) -> 'w cde
(* new uninitialized array, of the base type
*)
val new_uarray : 'a tbase -> int -> ('a array cde -> 'w cde) -> 'w cde

(* Inquiries. They are best effort *)
(* Is value statically known: known at code-generation time *)
(* It may always return false *)
val is_static : 'a cde -> bool
(* Is value dependent on something introduced by the library itself?
   For example, dependent on letl identifier, etc.
   It may always return true.
 *)
val is_fully_dynamic : 'a cde -> bool
