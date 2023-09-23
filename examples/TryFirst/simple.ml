(* The first, simplest and commonest examples of using Strymonas, 
   with detailed explanations

   These examples are meant to be executed (that is, #used) from the top-level 
   BER MetaOCaml.
*)
(* This is how this file is meant to be used
#use "simple.ml";;
*)

(* Add the directory with interface files and load the library *)
#directory "../../lib";;
#directory "../../lib/backends/Trx";;
#load "stream.cma";;

(* Select the MetaOCaml backend, to generate OCaml code *)
module C = Backends.MetaOCaml

(* Open the strymonas library: higher-level interface, which see
   ../../lib/stream_cooked.mli
*)
open Stream_cooked_fn.Make(C)

(* The first example (also the first example in the paper).
   See the paper (OCaml 22 paper) for explanation
 *)
let ex1 = iota C.(int 1) |> map C.(fun e -> e * e)
(* val ex1 : int cstream = <abstr> *)

(* The following sum is for illustration only: strymonas already provides this
   combinator, under the name sum_int, defined as below.
*)
let sum = fold C.(+) C.(int 0)
(* val sum : int cstream -> int Raw.stm = <fun> *)

let ex2 = ex1 |> filter C.(fun e -> e mod (int 17) > int 7) 
                   |> take C.(int 10) |> sum
(*
  val ex2 : int Raw.stm =
  C.Stm .<
   let v_1 = Stdlib.ref 0 in
   (let v_2 = Stdlib.ref 10 in
    let v_3 = Stdlib.ref 1 in
    while (! v_2) > 0 do
      let t_4 = ! v_3 in
      Stdlib.incr v_3;
      (let t_5 = t_4 * t_4 in
       if (t_5 mod 17) > 7 then (Stdlib.decr v_2; v_1 := ((! v_1) + t_5)))
      done);
   ! v_1>. 
*)

(* generate code *)
let ex2_code = C.to_code ex2
(*
val ex2_code : int code = .<
  let v_1 = Stdlib.ref 0 in
  (let v_2 = Stdlib.ref 10 in
   let v_3 = Stdlib.ref 1 in
   while (! v_2) > 0 do
     let t_4 = ! v_3 in
     Stdlib.incr v_3;
     (let t_5 = t_4 * t_4 in
      if (t_5 mod 17) > 7 then (Stdlib.decr v_2; v_1 := ((! v_1) + t_5)))
     done);
  ! v_1>. 
*)

(* The code may be written to a file, to compile later *)
let () =
  let file_name = "/var/tmp/ex2.ml" in
  let cout = open_out file_name in
  let ppf = Format.formatter_of_out_channel cout in
  Codelib.print_code ppf ex2_code; Format.fprintf ppf "%!"; close_out cout

(* Or, it can be run right away *)
let[@warning "-8"] 853 = Runcode.run ex2_code


(* Next example:
   Computing dot-product of two arrays in scope *)
let a1 = [|1;2;3|] and a2 = [|4;5;6|]
let ex_dot1 = zip_with C.( * ) (of_int_array a1) (of_int_array a2) |> sum |>
   C.to_code
(*
val ex_dot1 : int code = .<
  let t_7 = [|1;2;3|] in
  let t_8 = [|4;5;6|] in
  let v_6 = Stdlib.ref 0 in
  for i_9 = 0 to 2 do
    (let el_10 = Stdlib.Array.get t_7 i_9 in
     let el_11 = Stdlib.Array.get t_8 i_9 in
     v_6 := ((! v_6) + (el_10 * el_11)))
  done;
  ! v_6>. 
*)
let[@warning "-8"] 32 = Runcode.run ex_dot1

(* In ex_dot1, the arrays to work on were referred directly in the generator;
   the generated code had these arrays as literals.
   One can write a bit more general pipeline, parameterized by arrays
   (values of the type [int arr], to be precise)
*)
let ex_dot (arr1,arr2) = zip_with C.( * ) (of_arr arr1) (of_arr arr2) |> sum
(* val ex_dot : int Raw.arr * int Raw.arr -> int Raw.stm = <fun> *)

(* One can still use the literal arrays as before: *)
let ex_dot1' =
  C.to_code @@
  C.new_array C.tint (Array.map C.int a1) @@ fun arr1 ->
  C.new_array C.tint (Array.map C.int a2) @@ fun arr2 ->
  ex_dot (arr1,arr2)
(*
val ex_dot1' : int code = .<
  let t_18 = [|1;2;3|] in
  let t_19 = [|4;5;6|] in
  let v_20 = Stdlib.ref 0 in
  for i_21 = 0 to 2 do
    (let el_22 = Stdlib.Array.get t_18 i_21 in
     let el_23 = Stdlib.Array.get t_19 i_21 in
     v_20 := ((! v_20) + (el_22 * el_23)))
  done;
  ! v_20>. 
*)
let[@warning "-8"] 32 = Runcode.run ex_dot1'

(* Alternatively, we may generate a function taking two arrays *)

let ex_dot_fn = C.two_arg_fun ex_dot
(*
val ex_dot_fn : (int array * int array -> int) code = .<
  fun (arg1_18, arg2_19) ->
    let v_20 = Stdlib.ref 0 in
    for i_21 = 0 to
      (if (Stdlib.Array.length arg1_18) < (Stdlib.Array.length arg2_19)
       then Stdlib.Array.length arg1_18
       else Stdlib.Array.length arg2_19) - 1
      do
      (let el_22 = Stdlib.Array.get arg1_18 i_21 in
       let el_23 = Stdlib.Array.get arg2_19 i_21 in
       v_20 := ((! v_20) + (el_22 * el_23)))
    done;
    ! v_20>.
*)

(* which me may apply to any two suitable arrays *)
let[@warning "-8"] 32 = 
  let fn = Runcode.run ex_dot_fn in
  fn (a1,a2)


(* Nesting: Cartesian Product of two streams *)
(* We now see tupling: the stream of tuples rather than a stream of
   base-type items.
   To deal with such general streams we sometimes have to use functions
   from that Raw interface.
 *)
let cart (s1,s2) =
  s1 |> flat_map (fun e1 -> s2 |> Raw.map_raw' (fun e2 -> (e1,e2)))
(* val cart : 'a cstream * 'b stream -> ('a Raw.exp * 'b) stream = <fun> *)

let ex_cart (a1,a2) =
  cart (of_arr a1, of_arr a2) |>
  iter C.(fun (e1,e2) -> seq (print_int e1) (C.F64.print e2))
(*
val ex_cart : int Raw.arr * float Raw.arr -> unit Raw.stm = <fun>
*)

(* The generated code shows nested loops, predictably
 *)
let cart_fn = C.two_arg_fun ex_cart
(*
val cart_fn : (int array * float array -> unit) code = .<
  fun (arg1_24, arg2_25) ->
    for i_26 = 0 to (Stdlib.Array.length arg1_24) - 1 do
      let el_27 = Stdlib.Array.get arg1_24 i_26 in
      for i_28 = 0 to (Stdlib.Array.length arg2_25) - 1 do
        let el_29 = Stdlib.Array.get arg2_25 i_28 in
        (Stdlib.Format.print_int el_27; Stdlib.Format.force_newline ());
        Stdlib.Format.printf
          (CamlinternalFormatBasics.Format
             ((CamlinternalFormatBasics.Float
                 ((CamlinternalFormatBasics.Float_flag_,
                    CamlinternalFormatBasics.Float_g),
                   CamlinternalFormatBasics.No_padding,
                   (CamlinternalFormatBasics.Lit_precision 17),
                   (CamlinternalFormatBasics.Formatting_lit
                      (CamlinternalFormatBasics.Force_newline,
                        CamlinternalFormatBasics.End_of_format)))),
               "%.17g@\n")) el_29
      done
    done>.
*)

let () = 
  let fn = Runcode.run cart_fn in
  fn ([|1;2;3|], [|1.0;2.0|]);
  Format.print_flush ()

(* The output goes to stdout.
   What if we want the output as a list of tuples?
   For that, we have to use an extended backend, which permits lists
   and, in general, arbitrary OCaml expressions, in MetaOCaml brackets.
*)

let cart_fn' =
  let module C = Backends.MetaOCamlExt in
  let open Stream_cooked_fn.Make(C) in
  (* if we made cart a functor earlier, there would be no need to rewrite
     it below.
   *)
  let cart (s1,s2) =
   s1 |> flat_map (fun e1 -> s2 |> Raw.map_raw' (fun e2 -> (e1,e2))) in
  let ex_cart (a1,a2) =
   cart (of_arr a1, of_arr a2) |>
   Raw.map_raw' (fun (x,y) -> C.pair x y) |>
   (* fold is a statement. If we need to process its result, we need fold_ *)
   fold_ Desc.Single (fun l x -> C.cons x l) (C.nil ()) 
     (fun x -> C.cde_app1 .<List.rev>. x |> C.ret) in
   C.two_arg_fun ex_cart

(*
 val cart_fn' :
  ('_weak1 array * '_weak2 array -> ('_weak1 * '_weak2) list) code = .<
  fun (arg1_30, arg2_31) ->
    let v_32 = Stdlib.ref [] in
    for i_33 = 0 to (Stdlib.Array.length arg1_30) - 1 do
      (let el_34 = Stdlib.Array.get arg1_30 i_33 in
       for i_35 = 0 to (Stdlib.Array.length arg2_31) - 1 do
         let el_36 = Stdlib.Array.get arg2_31 i_35 in
         v_32 := ((el_34, el_36) :: (! v_32))
       done)
    done;
    Stdlib.List.rev (! v_32)>.
*)

let[@warning "-8"] 
 [(1, 1.); (1, 2.); (2, 1.); (2, 2.); (3, 1.); (3, 2.)]
 =
  let fn = Runcode.run cart_fn' in
  fn ([|1;2;3|], [|1.0;2.0|])


(* With filtering, cartesian product becomes join *)

let join1 =
  C.two_arg_fun @@ fun (a1,a2) ->
  cart (of_arr a1, of_arr a2) |>
  Raw.filter_raw C.(fun (e1,e2) -> e1 = F64.truncate e2) |>
  iter C.(fun (e1,e2) -> seq (print_int e1) (F64.print e2))

(*
val join1 : (int array * float array -> unit) code = .<
  fun (arg1_37, arg2_38) ->
    for i_39 = 0 to (Stdlib.Array.length arg1_37) - 1 do
      let el_40 = Stdlib.Array.get arg1_37 i_39 in
      for i_41 = 0 to (Stdlib.Array.length arg2_38) - 1 do
        let el_42 = Stdlib.Array.get arg2_38 i_41 in
        if el_40 = (Stdlib.truncate el_42)
        then
          ((Stdlib.Format.print_int el_40; Stdlib.Format.force_newline ());
           Stdlib.Format.printf
             (CamlinternalFormatBasics.Format
                ((CamlinternalFormatBasics.Float
                    ((CamlinternalFormatBasics.Float_flag_,
                       CamlinternalFormatBasics.Float_g),
                      CamlinternalFormatBasics.No_padding,
                      (CamlinternalFormatBasics.Lit_precision 17),
                      (CamlinternalFormatBasics.Formatting_lit
                         (CamlinternalFormatBasics.Force_newline,
                           CamlinternalFormatBasics.End_of_format)))),
                  "%.17g@\n")) el_42)
      done
    done>.
*)



(* One example of stream nesting: database join

   Suppose T1 is a table of string * int tuples, and T2 is a
   table of int * float tuples.
   The following computes their natural join on the int column,
   followed by filtering.
   We thus implement something like the following:
   SELECT T1.1, 2*T2.2 FROM T1, T2 WHERE T1.2 = T2.1 AND T2.2 > 5.0

  The join is the nested loop join
*)

let join_fn =
  let module C = Backends.MetaOCamlExt in
  let open Stream_cooked_fn.Make(C) in
  (* if we made cart a functor earlier, there would be no need to rewrite
     it below.
   *)
  let cart (s1,s2) =
   s1 |> flat_map (fun e1 -> s2 |> Raw.map_raw' (fun e2 -> (e1,e2))) in
  let join (t1,t2) =
   cart (of_arr t1, of_arr t2) |>
   (* WHERE clauses *)
   Raw.filter_raw C.(fun (e1,e2) -> snd e1 = fst e2) |>
   Raw.filter_raw C.(fun (e1,e2) -> F64.truncate (snd e2) > int 5) |>
   (* SELECTion *)
   Raw.map_raw' C.(fun (e1,e2) -> F64.(pair (fst e1) (snd e2 *. lit 2.))) |>
   (* Accumulation of the result *)
   fold_ Desc.Single (fun l x -> C.cons x l) (C.nil ()) 
     (fun x -> C.cde_app1 .<List.rev>. x |> C.ret) in
   C.two_arg_fun join

(* 
val join_fn :
  (('_weak3 * int) array * (int * float) array -> ('_weak3 * float) list)
  code = .<
  fun (arg1_8, arg2_9) ->
    let v_10 = Stdlib.ref [] in
    for i_11 = 0 to (Stdlib.Array.length arg1_8) - 1 do
      (let el_12 = Stdlib.Array.get arg1_8 i_11 in
       for i_13 = 0 to (Stdlib.Array.length arg2_9) - 1 do
         let el_14 = Stdlib.Array.get arg2_9 i_13 in
         if
           ((Stdlib.snd el_12) = (Stdlib.fst el_14)) &&
             ((Stdlib.truncate (Stdlib.snd el_14)) > 5)
         then
           v_10 := (((Stdlib.fst el_12), ((Stdlib.snd el_14) *. 2.)) ::
             (! v_10))
       done)
    done;
    Stdlib.List.rev (! v_10)>.
*)

let[@warning "-8"] 
 [("b",24.)]
 =
  let fn = Runcode.run join_fn in
  fn ([| ("a",1); ("b",2); ("c",3)|], [|(1,1.0);(2,12.0); (4,20.)|])
  

(* Strymonas also supports a limited form of forking: multiple consumers.
   For example, here is how we can compute min and max
 *)
let minmax_fn =
   C.one_arg_fun @@ fun a ->
   of_arr a |> fold_ Desc.Tuple 
      C.(fun (mn,mx) a -> (imin mn a, imax mx a)) C.(int max_int, int min_int)
      C.(fun (mn,mx) -> seq (print_int mn) (print_int mx))

let () = Runcode.run minmax_fn [|10;-100;0;500;100|];
    Format.print_flush ()


(* The issue of sharing.
   Since the metalanguage (OCaml) has let-expressions, we may share 
   streams, it seems.
*)

let same_st = 
  C.to_code @@
  let sts = iota C.(int 1) |> take C.(int 10) in
  zip_with C.(+) sts (sts |> filter C.(fun x -> x mod int 3 = int 0)) |>
  sum
(* However, if we look at the generated code below, we see two copies of
   the stream sts. The let-expression in the metalanguage shares
   *generators*, not the generated code (stream constructors, not streams
   themselves).

val same_st : int code = .<
  let v_20 = Stdlib.ref 0 in
  (let v_21 = Stdlib.ref 10 in
   let v_22 = Stdlib.ref 1 in
   let v_23 = Stdlib.ref 10 in
   let v_24 = Stdlib.ref 1 in
   while ((! v_23) > 0) && ((! v_21) > 0) do
     Stdlib.decr v_23;
     (let t_25 = ! v_24 in
      Stdlib.incr v_24;
      if (t_25 mod 3) = 0
      then
        (Stdlib.decr v_21;
         (let t_26 = ! v_22 in
          Stdlib.incr v_22; v_20 := ((! v_20) + (t_26 + t_25)))))
     done);
  ! v_20>. 
*)
let[@warning "-8"] 24 = Runcode.run same_st


(* Big test from the paper *)
let paper_test = 
 let square x = C.(x * x) and
     even x = C.(x mod (int 2) = int 0) in
 Raw.zip_raw
   (* First stream to zip *)
   ([|0;1;2;3|] |> of_int_array
     |> map square
     |> take (C.int 12)
     |> filter even
     |> map square)
   (* Second stream to zip *)
   (iota (C.int 1)
     |> flat_map (fun x -> 
          iota C.(x+int 1) |> take (C.int 3))
     |> filter even)
   |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

let paper_test_code = C.to_code paper_test

(*
val paper_test_code : unit code = .<
  let t_28 = [|0;1;2;3|] in
  let v_27 = Stdlib.ref 12 in
  let v_29 = Stdlib.ref 1 in
  let v_33 = Stdlib.ref 0 in
  while ((! v_27) > 0) && ((! v_33) < 4) do
    let t_34 = ! v_29 in
    Stdlib.incr v_29;
    (let v_35 = Stdlib.ref 3 in
     let v_36 = Stdlib.ref (t_34 + 1) in
     while ((! v_35) > 0) && (((! v_27) > 0) && ((! v_33) < 4)) do
       Stdlib.decr v_35;
       (let t_37 = ! v_36 in
        Stdlib.incr v_36;
        if (t_37 mod 2) = 0
        then
          (let v_38 = Stdlib.ref true in
           while ! v_38 do
             ((let el_39 = Stdlib.Array.get t_28 (! v_33) in
               let t_40 = el_39 * el_39 in
               Stdlib.decr v_27;
               if (t_40 mod 2) = 0
               then
                 (let t_41 = t_40 * t_40 in
                  v_38 := false;
                  (Stdlib.Format.print_int t_41;
                   Stdlib.Format.force_newline ());
                  Stdlib.Format.print_int t_37;
                  Stdlib.Format.force_newline ()));
              Stdlib.incr v_33);
             v_38 := ((! v_38) && (((! v_27) > 0) && ((! v_33) < 4))) done))
       done)
    done>.
*)

let () = print_endline "All done"
;;
