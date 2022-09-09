(* The first, simplest and commonest examples of using Strymonas, 
   with detailed explanations

   This example is for generating C. It does not require MetaOCaml
*)

(* Evaluate the following if working at the top level of OCaml

#directory "../../lib";;
#directory "../../lib/backends/C";;
#load "stream_ocaml.cma";;
*)

(* C backend *)

(* The following functor is quoted from the module Backends. So we could have
just said 
   module C = Backends.C

However, Backends module also contains MetaOCaml backends and hence needs
MetaOCaml to compile it. We want the present file to be compilable by
OCaml only.
*)

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

  (* only for basic types *)
  let run : 'a cde -> 'a = fun x -> dyn x |> C.run
  let run_capture_output x = C.run_capture_output (dyn x)
  let run_output_to_command : unit cde -> string -> unit = fun x -> C.run_output_to_command (dyn x)  
end
module C = CGen(C_cde)

(* Again, Open the strymonas library: higher-level interface, which see
   ../../lib/stream_cooked.mli
*)
open Stream_cooked_fn.Make(C)

(* The first example (also the first example in the paper) as before.
   But now C stands for the C backend
   See the paper (OCaml 22 paper) for explanation
 *)
let ex1 = iota C.(int 1) |> map C.(fun e -> e * e)
(* val ex1 : int cstream = <abstr> *)

(* The following sum is for illustration only: strymonas already provides this
   combinator, under the name sum_int, defined as below.
*)
let sum = fold C.(+) C.(int 0)
(* val sum : int cstream -> int cde = <fun> *)

let ex2 = ex1 |> filter C.(fun e -> e mod (int 17) > int 7) 
                   |> take C.(int 10) |> sum

(* The generated code *)
let _ = C.print ~name:"fn" ex2

(*
int64_t fn()
{
   int64_t v_1 = 0;
   int64_t v_2 = 10;
   int64_t v_3 = 1;
   while (v_2 > 0)
   {
      int64_t t_4;
      int64_t t_5;
      t_4 = v_3;
      v_3++;
      t_5 = t_4 * t_4;
      if ((t_5 % 17) > 7)
      {
         v_2--;
         v_1 = v_1 + t_5;
      }
   }
   return v_1;}
*)

(* We can compile it, and run capturing its output *)

let[@warning "-8"] 853 = C.run ex2

let () = print_endline "All done"
;;
