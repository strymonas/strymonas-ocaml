(* Abstract interface for code generation 

   Building top-level functions and printing/running them
*)

include module type of Cde_ex

val ident : string                      (* identifier of the backend *)

(* Building top-level procedures *)

type 'a proc_t

val nullary_proc : 'a stm -> 'a proc_t
val arg_base : ?name:string -> 'a tbase -> ('a exp -> 'b proc_t) -> 
  ('a -> 'b) proc_t
val arg_array : ?name:string -> ?mutble:bool -> 
  int exp ->                            (* length *)
  'a tbase -> ('a arr -> 'b proc_t) -> ('a array -> 'b) proc_t

val pp_proc : ?name:string -> Format.formatter -> 'a proc_t -> unit

(* The following is often used in debugging and testing *)
val print_code : ?name:string -> 'a stm -> unit
val run : 'a stm -> 'a
val run_capture_output : unit stm -> Scanf.Scanning.in_channel

