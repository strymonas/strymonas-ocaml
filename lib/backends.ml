(* Convinient functions for selecting a backend *)

(* MetaOCaml backend

   This backend implements the `least-common denominator' of backends: 
   user actions implemented with this backend would work as they are
   if we switch to any other backend, like C.

   Beside implementing the backend interface, the module offers procedures
   to actually extract the generated Ocaml code, so it could be saved
   or run. 
*)

module MetaOCaml = struct
  include Pk_cde.Make(Trx_code)

  (* return the generated code *)
  let to_code : 'a stm -> 'a code = dyn_stm
  (* Historically used in benchmarks and early tests *)
  let one_arg_fun : ('a arr -> 'b stm) -> ('a array -> 'b) code
      = fun f -> 
        .<fun arg1 -> 
          .~(dyn_stm @@ f (inj_global .<Array.length arg1>.,.<arg1>.))>.
  let two_arg_fun : ('a arr * 'b arr -> 'c stm) -> 
                    ('a array * 'b array -> 'c) code 
      = fun f -> 
        .<fun (arg1,arg2) -> 
          .~(dyn_stm @@ f ((inj_global .<Array.length arg1>.,.<arg1>.), 
                           (inj_global .<Array.length arg2>.,.<arg2>.)))>.
end

(* The following is an extended version of MetaOCaml backend for OCaml-specific
   user actions. It permits arbitrary MetaOCaml quotations in user actions.
   This backend can no longer be swapped for C backend.
*)
module MetaOCamlExt = struct
  include MetaOCaml

  let of_code  : 'a code -> 'a exp = inj

  (* Convenience: Lists and Pairs *)
  let nil  : unit -> 'a list exp = fun () -> inj_global Trx_code.nil
  let cons : 'a exp -> 'a list exp -> 'a list exp = fun x y -> 
    inj2 Trx_code.cons x y
    
  let pair : 'a exp -> 'b exp -> ('a * 'b) exp = fun x y -> 
    inj2 Trx_code.pair x y
  let cde_app1 : ('a -> 'b) code -> 'a exp -> 'b exp = fun cde ->
    inj1 ((Trx_code.make_ff1 cde).invoke)
  let fst : ('a * 'b) exp -> 'a exp = fun x -> cde_app1 .<fst>. x
  let snd : ('a * 'b) exp -> 'b exp = fun x -> cde_app1 .<snd>. x
end


(* C backend *)
module C = Pk_cde.Make(C_cde)
