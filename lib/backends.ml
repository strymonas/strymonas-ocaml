(* Convinient functions for selecting a backend *)

(* MetaOCaml backend

   This backend implements the `least-common denominator' of backends: 
   user actions implemented with this backend would work as they are
   if we switch to any other backend, like C.

   Beside implementing the backend interface, the module offers procedures
   to actually extract the generated Ocaml code, so it could be saved
   or run. 
*)

module MetaOCamlGen(C: module type of Trx_code) = struct
  include Pk_cde.Make(C)

  let ident = "MetaOCaml"

  let print x =
    Codelib.print_code Format.std_formatter (dyn x);
    Format.fprintf Format.std_formatter "@." 

  (* return the generated code *)
  let to_code : 'a cde -> 'a code = dyn
  let one_arg_fun : ('a cde -> 'b cde) -> ('a -> 'b) code
      = fun f -> .<fun arg1 -> .~(dyn @@ f (inj_global .<arg1>.))>.
  let two_arg_fun : ('a cde * 'b cde -> 'c cde) -> ('a * 'b -> 'c) code 
      = fun f -> .<fun (arg1,arg2) -> 
                     .~(dyn @@ f (inj_global .<arg1>., inj_global .<arg2>.))>.
end
module MetaOCaml = MetaOCamlGen(Trx_code)

(* The following is an extended version of MetaOCaml backend for OCaml-specific
   user actions. It permits arbitrary MetaOCaml quotations in user actions.
   This backend can no longer be swapped for C backend.
*)
module MetaOCamlExtGen(C: module type of Trx_code) = struct
  include MetaOCamlGen(C)

  let of_code  : 'a code -> 'a cde = inj
  let with_cde : ('a code -> 'b cde) -> 'a cde -> 'b cde = fun f x -> f @@ dyn x
  (* Apply an arbitrary one-argument OCaml function *)
  let cde_app1 : ('a -> 'b) code -> 'a cde -> 'b cde = fun f x ->
    inj @@ .<.~f .~(dyn x)>.
  let cde_app2 : ('a -> 'b -> 'c) code -> 'a cde -> 'b cde -> 'c cde 
    = fun f x y ->
    inj @@ .<.~f .~(dyn x) .~(dyn y)>.

  (* Convenience: Lists and Pairs *)
  let nil  : unit -> 'a list cde = fun () -> inj_global C.nil
  let cons : 'a cde -> 'a list cde -> 'a list cde = fun x y -> inj2 C.cons x y
    
  let pair : 'a cde -> 'b cde -> ('a * 'b) cde = fun x y -> inj2 C.pair x y
  let fst : ('a * 'b) cde -> 'a cde = fun x -> cde_app1 .<fst>. x
  let snd : ('a * 'b) cde -> 'b cde = fun x -> cde_app1 .<snd>. x
end
module MetaOCamlExt = MetaOCamlExtGen(Trx_code)


(* C backend *)
module CGen(C: module type of C_cde) = struct
  include Pk_cde.Make(C)

  let ident = "C"

  let print ?name x =
    C.nullary_fun ?name Format.std_formatter (dyn x);
    Format.fprintf Format.std_formatter "@." 

  let print_one_array : string -> Format.formatter -> 
  'a tbase -> ('a array cde -> 'b cde) -> unit = fun name ppf tp body ->
    C.one_array_fun ~name ppf tp (fun arr -> inj_global arr |> body |> dyn);
    Format.fprintf ppf "@." 
  
  let print_two_array : string -> Format.formatter -> 
  'a tbase * 'b tbase -> 
  ('a array cde * 'b array cde -> 'c cde) -> unit = fun name ppf tps body ->
    C.two_array_fun ~name ppf tps (fun (arr1,arr2) -> 
      (inj_global arr1, inj_global arr2) |> body |> dyn);
    Format.fprintf ppf "@." 

  let run_capture_output x = C.run_capture_output (dyn x)
  let run_output_to_command : unit cde -> string -> unit = fun x -> C.run_output_to_command (dyn x)  
end
module C = CGen(C_cde)

module CExtGen(C: module type of C_cde) = struct
  include CGen(C)

  let cde_app1 : string -> 'b tbase -> 'a cde -> 'b cde =
    fun fname ret_typ x ->
      inj @@ C.cde_app1 fname ret_typ (dyn x)
  let cde_app2 : string -> 'c tbase -> 'a cde -> 'b cde -> 'c cde =
    fun fname ret_typ x y ->
      inj @@ C.cde_app2 fname ret_typ (dyn x) (dyn y)
end
module CExt = CExtGen(C_cde)
