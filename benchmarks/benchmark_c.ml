(* Generating code for C benchmarks *)

(*
#directory "../lib";;
#directory "../lib/backends/Trx";;
#directory "../lib/backends/C";;
#load "stream.cma";;
*)

module type cde_ex     = module type of Cde_ex

module CCodePV = struct
  include Pk_cde.Make(C_cde)

  let print_one_array : string -> Format.formatter -> 
  'a tbase -> ('a arr -> 'b stm) -> unit = fun name ppf tp body ->
    (arg_base tint @@ fun n -> 
     arg_array n tp @@ fun a -> body a |> nullary_proc) 
    |> pp_proc ~name ppf; 
    Format.fprintf ppf "@." 
  let print_two_array : string -> Format.formatter -> 'a tbase * 'b tbase -> 
  ('a arr * 'b arr -> 'c stm) -> unit = fun name ppf (tp1,tp2) body ->
    (arg_base tint @@ fun n1 -> arg_array n1 tp1 @@ fun a1 -> 
     arg_base tint @@ fun n2 -> arg_array n2 tp2 @@ fun a2 -> 
     body (a1,a2) |> nullary_proc) 
    |> pp_proc ~name ppf; 
    Format.fprintf ppf "@." 
end

module C = CCodePV
module Raw = Stream_raw_fn.Make(C)
open Stream_cooked_fn.Make_ex(C)(Raw)

type byte = int                        (* element of an outer stream *)
let byte_max = 255

let encode : bool cstream -> byte cstream = fun st ->
  Raw.initializing_ref C.(int 0) @@ fun zeros_count ->
  st |> Raw.map_raw ~linear:false (fun el k ->
    let open C in
    letl (dref zeros_count) @@ fun zeros ->
    if_ el (seq (zeros_count := int 0) (k zeros)) @@
    seq (zeros_count := zeros + int 1) @@
    if1 (dref zeros_count = int byte_max) @@
    seq (zeros_count := int 0) (k (int byte_max))
  )

(* advanced and more interesting use of flat_map *)
let decode : byte cstream -> bool cstream = fun st ->
  st |> flat_map (fun el ->
    Raw.pull_array C.(el + int 1) @@ fun i k ->
      let open C in
      if_ (i < el) (k (bool false))
          (if1 (i < int byte_max) (k (bool true))))

let generate ppf =
  let () = C.print_one_array "sum" ppf C_cde.tint @@ fun arr ->
      of_arr arr
      |> sum_int
  in
  let () = C.print_one_array "sum_squares" ppf C_cde.tint @@ fun arr ->
       of_arr arr
       |> map C.(fun x -> x * x)
       |> sum_int
  in
  let () = C.print_one_array "maps" ppf C_cde.tint @@ fun arr ->
      of_arr arr
      |> map C.(fun x -> x * int 1)
      |> map C.(fun x -> x * int 2)
      |> map C.(fun x -> x * int 3)
      |> map C.(fun x -> x * int 4)
      |> map C.(fun x -> x * int 5)
      |> map C.(fun x -> x * int 6)
      |> map C.(fun x -> x * int 7)
      |> sum_int
  in
  let () = C.print_one_array "filters" ppf C_cde.tint @@ fun arr ->
     of_arr arr
     |> filter C.(fun x -> x > int 1)
     |> filter C.(fun x -> x > int 2)
     |> filter C.(fun x -> x > int 3)
     |> filter C.(fun x -> x > int 4)
     |> filter C.(fun x -> x > int 5)
     |> filter C.(fun x -> x > int 6)
     |> filter C.(fun x -> x > int 7)
     |> sum_int
  in
  let () = C.print_one_array "sum_squares_even" ppf C_cde.tint @@ fun arr ->
     of_arr arr
     |> filter C.(fun x -> x mod (int 2) = int 0)
     |> map C.(fun x -> x * x)
     |> sum_int
  in
  let () = C.print_two_array "cart" ppf (C_cde.tint,C_cde.tint) @@ 
  fun (arr1,arr2) ->
       of_arr arr1
       |> flat_map (fun x -> of_arr arr2 |> map C.(fun y -> x * y))
       |> sum_int
  in
  let () = C.print_two_array "dot_product" ppf (C_cde.tint,C_cde.tint) @@ 
  fun (arr1,arr2)->
     zip_with C.( * ) (of_arr arr1) (of_arr arr2)
     |> sum_int
  in
  let () = C.print_two_array "flatmap_after_zipwith" ppf 
      (C_cde.tint,C_cde.tint) @@ 
  fun (arr1,arr2) ->
     zip_with C.( + ) (of_arr arr1) (of_arr arr1)
     |> flat_map (fun x -> of_arr arr2|> map C.(fun el -> el + x))
     |> sum_int
  in
  let () = C.print_two_array "zipwith_after_flatmap" ppf 
      (C_cde.tint,C_cde.tint) @@ fun (arr1,arr2) ->
     of_arr arr1
     |> flat_map (fun x -> of_arr arr2 |> map C.(fun y -> y + x))
     |> zip_with C.( + ) (of_arr arr1)
     |> sum_int
  in
  let () = C.print_two_array "flat_map_take" ppf (C_cde.tint,C_cde.tint) @@ 
  fun (arr1,arr2)->
        of_arr arr1
        |> flat_map (fun x -> of_arr arr2 |> map C.(fun y -> x * y))
        |> take (C.int 20_000_000)
        |> sum_int
  in
  let () = C.print_two_array "zip_filter_filter" ppf 
      (C_cde.tint,C_cde.tint) @@ fun (arr1,arr2) ->
  zip_with C.( + )
   (of_arr arr1 |> filter C.(fun x -> x > int 7))
   (of_arr arr2 |> filter C.(fun x -> x > int 5))
  |> sum_int
  in
  let () = C.print_two_array "zip_flat_flat" ppf (C_cde.tint,C_cde.tint) @@ 
  fun (arr1,arr2) ->
  zip_with C.( + )
   (of_arr arr1 |> 
     flat_map (fun x -> of_arr arr2 |> map C.(fun y -> x * y)))
   (of_arr arr2 |> 
     flat_map (fun x -> of_arr arr1 |> map C.(fun y -> x - y)))
  |> take (C.int 200_000_000)
  |> sum_int
  in 
  let () = C.print_two_array "decoding" ppf (C_cde.tint,C_cde.tint) @@ 
  fun (arr1,arr2) ->
    zip_with C.(||) (of_arr arr1 |> decode) (of_arr arr2 |> decode)
    |> map C.(fun x -> cond x (int 1) (int 0))
    |> sum_int
  in
  ()

let () = generate Format.std_formatter

let () =
  let code_file = "/tmp/bench.c" in
  let c = open_out code_file in
  let ppf = Format.formatter_of_out_channel c in
  generate ppf;
  close_out c;
  Printf.printf "\nGenerated %s\n" code_file

;;
