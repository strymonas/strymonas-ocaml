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

(* The following module is quoted from the module Backends. So we could have
just said 
   module C = Backends.C

However, Backends module also contains MetaOCaml backends and hence needs
MetaOCaml to compile it. We want the present file to be compilable by
OCaml only.
*)

module C = Pk_cde.Make(C_cde)

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
let _ = C.print_code ~name:"fn" ex2

(*
int64_t fn(){
  int64_t x_1 = 0;
  int64_t x_2 = 10;
  int64_t x_3 = 1;
  while (x_2 > 0)
  {
    int64_t const t_4 = x_3;
    x_3++;
    int64_t const t_5 = t_4 * t_4;
    if ((t_5 % 17) > 7)
    {
      x_2--;
      x_1 = x_1 + t_5;
    }
  }
  return x_1;
}
*)

(* We can compile it, and run capturing its output *)

let[@warning "-8"] 853 = C.run ex2

let () = print_endline "All done"
;;
