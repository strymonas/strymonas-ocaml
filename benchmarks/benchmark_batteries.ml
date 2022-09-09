module Batteries = struct
   open BatEnum
   type 'a cde = 'a code
   type 'a stream_raw = 'a t
   type 'a stream = 'a t cde
 
   let lift_tr1 : (('a -> 'b ) -> 'a stream_raw -> 'c stream_raw) cde
               ->  ('a cde -> 'b cde) -> 'a stream -> 'c stream =
     fun tr f st -> .<.~tr (fun x -> .~(f .<x>.)) .~st>.
 
   (* let lift_tr2 : (('a -> 'b -> 'c) -> ('a stream_raw -> 'b stream_raw -> 'c stream_raw) )cde
               ->  ('a cde -> 'b cde -> 'c cde) -> 'a stream -> 'b stream -> 'c stream =
     fun tr f st1 st2 -> .<.~tr (fun x y -> .~(f .<x>. .<y>.)) .~st1 .~st2>. *)
 
 
   let of_arr : 'a array cde -> 'a stream = fun x -> .<BatArray.enum .~x>.
 
   let fold : ('z cde -> 'a cde -> 'z cde) -> 'z cde -> 'a stream -> 'z cde = 
     fun f z st -> .<fold (fun z x -> .~(f .<z>. .<x>.)) .~z .~st>.
 
   let map  : ('a cde -> 'b cde) -> 'a stream -> 'b stream = 
     fun f st -> lift_tr1 .<map>. f st
 
   let flat_map : ('a cde -> 'b stream) -> 'a stream -> 'b stream =
     fun f st -> lift_tr1 .<concat_map>. f st
 
   let filter   : ('a cde -> bool cde) -> 'a stream -> 'a stream =
     fun f st -> lift_tr1 .<filter>. f st
 
   let take     : int cde -> 'a stream -> 'a stream =
     fun n st -> .<take .~n .~st>.
 
   let zip_with : ('a cde -> 'b cde -> 'c cde) -> ('a stream -> 'b stream -> 'c stream) =
     fun f st1 st2 ->
     .<BatStream.map2 (fun x y -> .~(f .<x>. .<y>.)) (.~st1 |> BatStream.of_enum) (.~st2 |> BatStream.of_enum)
      |> BatStream.enum>.

   type byte = int
   let byte_max = 255
   let decode = fun st ->
     st |> flat_map (fun el -> .<
       unfold 0 (fun i ->
         if i < .~el then Some (false, i + 1)
         else (
          if i > .~el then None
          else  (
            if i < byte_max then Some (true, i + 1)
            else failwith "wrong sequence"
          )
         )
       )>.)
 end


module Benchmark_batteries = struct
   open Benchmark_types
   open Benchmark
   open Benchmark_abstract.Benchmark(Benchmark_abstract.CodeBasic)(Batteries)

   (* let maps_array = fun src -> .<
      .~src
      |> BatArray.map(fun x -> x * 1)
      |> BatArray.map(fun x -> x * 2)
      |> BatArray.map(fun x -> x * 3)
      |> BatArray.map(fun x -> x * 4)
      |> BatArray.map(fun x -> x * 5)
      |> BatArray.map(fun x -> x * 6)
      |> BatArray.map(fun x -> x * 7)
      |> BatArray.fold_left (fun z a -> z + a) 0 >.

   let filters_array = fun src -> .<
      .~src
      |> BatArray.filter(fun x -> x > 1)
      |> BatArray.filter(fun x -> x > 2)
      |> BatArray.filter(fun x -> x > 3)
      |> BatArray.filter(fun x -> x > 4)
      |> BatArray.filter(fun x -> x > 5)
      |> BatArray.filter(fun x -> x > 6)
      |> BatArray.filter(fun x -> x > 7)
      |> BatArray.fold_left (fun z a -> z + a) 0 >. *)

   (* faster than normal dotProduct *)
   (* let dotProduct_enum = fun (src1, src2) -> .<
      BatEnum.fold2 (fun z x y -> z + x * y) 0
                    (.~src1 |> BatArray.enum)
                    (.~src2 |> BatArray.enum) >. *)


   (* Arrays used for benchmarking *)
   let v     = .< Array.init 100_000_000 (fun i -> i mod 10) >.;;
   let vHi   = .< Array.init 10_000_000 (fun i -> i mod 10) >.;;
   let vLo   = .< Array.init 10 (fun i -> i mod 10) >.;;
   let vFaZ  = .< Array.init 10_000 (fun i -> i) >.;;
   let vZaF  = .< Array.init 10_000_000 (fun i -> i) >.;;

   let options = {
      repetitions = 20;
      final_f = (fun _ -> .<()>.);
   }

   let pr_int = {options with
     final_f = fun x -> .<Printf.printf ""; Printf.printf "Result %d\n" .~x>.}

   let check_int n = {options with
     final_f = fun x -> .<Printf.printf ""; assert (.~x = n) >.}

   let script =[|
      perfS  "sum_batteries" v sum options;
      perfS  "sumOfSquares_batteries" v sumOfSquares options;
      perfS  "sumOfSquaresEven_batteries" v sumOfSquaresEven options;
      perfS  "mapsMegamorphic_batteries" v maps options;
      (* perfS  "mapsMegamorphicArray_batteries" v maps_array options; *)
      perfS  "filtersMegamorphic_batteries" v filters options;
      (* perfS  "filtersMegamorphicArray_batteries" v filters_array options; *)
      perfS2 "cart_batteries" vHi vLo cart options;
      (* perfS2 "dotProduct_enum_batteries" vHi vHi dotProduct_enum options; *)
      perfS2 "dotProduct_batteries" vHi vHi dotProduct options;
      perfS2 "flatMapAfterZip_batteries" vFaZ vFaZ flatMap_after_zipWith options;
      perfS2 "zipAfterFlatMap_batteries" vZaF vZaF zipWith_after_flatMap options;
      perfS2 "flatMapTake_batteries" vHi vLo flat_map_take options;
      perfS2 "zipFilterFilter_batteries"   v vHi zip_filter_filter options;
      perfS2 "zipFlatMapFlatMap_batteries" v vLo zip_flat_flat options;
      perfS2 "runLengthDecoding_batteries" v v decoding options;
   |];;

   (* too slow *)
   let test = .<
    print_endline "Not checked yet";
    assert (.~(sum               v) == 450000000);
    assert (.~(sumOfSquares      v) == 2850000000);
    assert (.~(sumOfSquaresEven  v) == 1200000000);
    assert (.~(maps              v) == 2268000000000);
    assert (.~(filters           v) == 170000000);
    assert (.~(cart                  (vHi,  vLo))  == 2025000000);
    assert (.~(dotProduct            (vHi,  vHi))  == 285000000);
    assert (.~(flatMap_after_zipWith (vFaZ, vFaZ)) == 1499850000000);
    assert (.~(zipWith_after_flatMap (vZaF, vZaF)) == 99999990000000);
    assert (.~(flat_map_take         (vHi,  vLo))  == 405000000);
    assert (.~(zip_filter_filter     (v,    vHi))  == 64000000);
    assert (.~(zip_flat_flat         (v,    vLo))  == 3250000000);
    assert (.~(decoding              (v,    v))    == 100000000);
    print_endline "All done"
   >.
end

module M = Benchmark_batteries

let main () =
  let compiler = "ocamlfind ocamlopt -O2 -unsafe -nodynlink -package batteries -linkpkg util.cmx" in 
  match Sys.argv with
  | [|_;"test"|] ->
    Benchmark.run_natively M.test
      ~compiler
      (* ~save:true *)
  | _ ->
    Benchmark.run_script M.script
      ~compiler

let _ = main ()
