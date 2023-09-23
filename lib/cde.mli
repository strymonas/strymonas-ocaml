(* Abstract interface for code generation 

   Which code generation facilities we really need for this library

   This interface concerns the code generation used in strymonas
   itself. The `semantic actions' (user supplied code for mapping, etc)
   generally uses a richer interface: see exp_ex

   The object/target language is considered statement-based,
   distinguising expressions and statements (like C and many such
   languages do).
   For the justification of the design, see Generating C
*)

type 'a exp                             (* Abstract type of expressions *)

type 'a tbase                           (* Base types *)
val tbool : bool tbase                  (* More can/will be added later *)
val tint  : int tbase                   (* corresponds to C int, the `normal'
                                           or `fastest' int type. At least
                                           32-bit (usually 32-bit)
                                        *)
                                        (* Backends may provide more base types 
                                           unit is not a base type!
                                           In fact, there are no expressions
                                           of unit type
                                         *)

type 'a stm                             (* Statements. May produce values:
                                           see return in C
                                         *)

(* Local let, without movement. Only in statement context, like in C *)
val letl : 'a exp -> (('a exp -> 'w stm) -> 'w stm)
val glet : 'a exp -> 'a exp             (* possibly let-insertion, at some
                                           higher place. That is, movement
                                           is OK
                                         *)

val seq  : unit stm -> 'a stm -> 'a stm 
val unit : unit stm

val ret  : 'a exp -> 'a stm             (* Like return in C *)

(* Booleans *)
val bool : bool -> bool exp
val not  : bool exp -> bool exp
val (&&) : bool exp -> bool exp -> bool exp   (* shortcut eval *)
val (||) : bool exp -> bool exp -> bool exp

(* Integers *)
val int   : int -> int exp
val ( + ) : int exp -> int exp -> int exp
val ( - ) : int exp -> int exp -> int exp
val (mod) : int exp -> int exp -> int exp
val ( =)  : int exp -> int exp -> bool exp
val ( <)  : int exp -> int exp -> bool exp
val ( >)  : int exp -> int exp -> bool exp
val (<=)  : int exp -> int exp -> bool exp
val (>=)  : int exp -> int exp -> bool exp

(* It turns out that Stdlib.min is slow! It was responsible for
   the big slowdown in one of the benchmarks. It seems OCaml does not
   inline min and generates a function call instead.
 *)
val imin : int exp -> int exp -> int exp


(* Control operators *)
(* Separate if_ expression from if-statement *)
val cond : bool exp -> 'a exp -> 'a exp -> 'a exp
val if_  : bool exp -> unit stm -> unit stm -> unit stm
val if1  : bool exp -> unit stm -> unit stm

val for_ : int exp ->           (* exact lower bound *)
           upe:int exp ->       (* least upper bound, *exclusive* *)
           ?guard:bool exp ->   (* possibly a guard, terminate when false *)
           ?step:int exp ->     (* step *)
           (int exp -> unit stm) -> unit stm

val while_ : bool exp -> unit stm -> unit stm

(* Loop with continue: execute the body, which may request to be
   re-executed (like C `continue')
 *)
val cloop :
    ('a -> unit stm) ->        (* cloop's continuation *)
    bool exp option ->         (* possibly a guard. 
                                  It is true at the beginning *)
    (('a -> unit stm) -> unit stm) ->   (* body, in CPS, which may exit
                                           without calling its continuation,
                                           in which case the loop is re-executed
                                           provided the guard is still true.
                                         *)
    unit stm


(* Mutable variables *)
type 'a mut                     (* Mutable variables are NOT expressions *)
val newref  : 'a exp -> ('a mut -> 'w stm) -> 'w stm
(* Create an *uninitialized* reference cell. The first argument is
   not an initial value: it is just a hint to get the type.
   Therefore, the first argument may be unusable as the value.
   A backend may still use some other value of the same type as
   the initial value.
 *)
val newuref : 'a exp -> ('a mut -> 'w stm) -> 'w stm
val dref    :  'a mut -> 'a exp
val (:=)    :  'a mut -> 'a exp -> unit stm
val incr    : int mut -> unit stm
val decr    : int mut -> unit stm

(* Arrays *)
type 'a arr                     (* Arrays are not expressions *)
(* XXX Later consider constant arrays, mutable arrays and mutable
   extensible arrays
*)
val array_get' : 'a arr -> int exp -> 'a exp
(* It makes sense to combine it with letl *)
val array_get  : 'a arr -> int exp -> ('a exp -> 'w stm) -> 'w stm
val array_len  : 'a arr -> int exp
val array_set  : 'a arr -> int exp -> 'a exp -> unit stm
(* initialized non-empty array, immediately bound to a variable.
   Must be of a base type
 *)
val new_array  : 'a tbase -> 'a exp array -> ('a arr -> 'w stm) -> 'w stm
(* Array of the statically known size with statically known elements
   It should not be modified. If possible, it is allocated in the DATA
   segment
 *)
val new_static_array  : 'a tbase -> ('b -> 'a exp) -> 'b array -> 
  ('a arr -> 'w stm) -> 'w stm
(* new uninitialized array, of the base type
*)
val new_uarray : 'a tbase -> int -> ('a arr -> 'w stm) -> 'w stm

(* Inquiries. They are best effort *)
(* Is value statically known: known at code-generation time *)
(* It may always return false *)
val is_static : 'a exp -> bool
(* Is value dependent on something introduced by the library itself?
   For example, dependent on letl identifier, etc.
   It may always return true.
 *)
val is_fully_dynamic : 'a exp -> bool
