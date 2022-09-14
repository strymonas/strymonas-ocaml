module Iterb = struct
   open Iter

   type 'a cde = 'a code
   type 'a stream_raw = 'a t
   type 'a stream = 'a t cde
 
   let lift_tr1 : (('a -> 'b ) -> 'a stream_raw -> 'c stream_raw) cde
               ->  ('a cde -> 'b cde) -> 'a stream -> 'c stream =
     fun tr f st -> .<.~tr (fun x -> .~(f .<x>.)) .~st>.
 
   let lift_tr2 : (('a -> 'b -> 'c) -> ('a stream_raw -> 'b stream_raw -> 'c stream_raw) )cde
               ->  ('a cde -> 'b cde -> 'c cde) -> 'a stream -> 'b stream -> 'c stream =
     fun tr f st1 st2 -> .<.~tr (fun x y -> .~(f .<x>. .<y>.)) .~st1 .~st2>.
 
 
   let of_arr : 'a array cde -> 'a stream = fun x -> .<of_array .~x>.
 
   let fold : ('z cde -> 'a cde -> 'z cde) -> 'z cde -> 'a stream -> 'z cde = 
     fun f z st -> .<fold (fun z x -> .~(f .<z>. .<x>.)) .~z .~st>.
 
   let map  : ('a cde -> 'b cde) -> 'a stream -> 'b stream = 
     fun f st -> lift_tr1 .<map>. f st
 
   let flat_map : ('a cde -> 'b stream) -> 'a stream -> 'b stream =
     fun f st -> lift_tr1 .<flat_map>. f st
 
   let filter   : ('a cde -> bool cde) -> 'a stream -> 'a stream =
     fun f st -> lift_tr1 .<filter>. f st
 
   let take     : int cde -> 'a stream -> 'a stream =
     fun n st -> .<take .~n .~st>.
 
   let zip_with : ('a cde -> 'b cde -> 'c cde) -> ('a stream -> 'b stream -> 'c stream) =
     fun f st1 st2 -> failwith "unusable"

   type byte = int
   let byte_max = 255
   let decode = fun st ->
     st |> flat_map (fun el -> .<
       unfoldr (fun i ->
         if i < .~el then Some (false, i + 1)
         else (
          if i > .~el then None
          else  (
            if i < byte_max then Some (true, i + 1)
            else failwith "wrong sequence"
          )
         )
       ) 0>.)
 end


module Benchmark_iter = struct
   open Benchmark_types
   open Benchmark
   open Benchmark_abstract.Benchmark(Benchmark_abstract.CodeBasic)(Iterb)

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
      perfS  "sum_iter" v sum options;
      perfS  "sumOfSquares_iter" v sumOfSquares options;
      perfS  "sumOfSquaresEven_iter" v sumOfSquaresEven options;
      perfS  "mapsMegamorphic_iter" v maps options;
      perfS  "filtersMegamorphic_iter" v filters options;
      perfS2 "cart_iter" vHi vLo cart options;
      (* perfS2 "dotProduct_iter" vHi vHi dotProduct options;
      perfS2 "flatMapAfterZip_iter" vFaZ vFaZ flatMap_after_zipWith options;
      perfS2 "zipAfterFlatMap_iter" vZaF vZaF zipWith_after_flatMap options; *)
      perfS2 "flatMapTake_iter" vHi vLo flat_map_take options;
      (* perfS2 "zipFilterFilter_iter"   v vHi zip_filter_filter options;
      perfS2 "zipFlatMapFlatMap_iter" v vLo zip_flat_flat options;
      perfS2 "runLengthDecoding_iter" v v decoding options; *)
   |];;

   let test = .<
    print_endline "Last checked: Sep 9, 2022";
    assert (.~(sum               v) == 450000000);
    assert (.~(sumOfSquares      v) == 2850000000);
    assert (.~(sumOfSquaresEven  v) == 1200000000);
    assert (.~(maps              v) == 2268000000000);
    assert (.~(filters           v) == 170000000);
    assert (.~(cart                  (vHi,  vLo))  == 2025000000);
    (* assert (.~(dotProduct            (vHi,  vHi))  == 285000000);
    assert (.~(flatMap_after_zipWith (vFaZ, vFaZ)) == 1499850000000);
    assert (.~(zipWith_after_flatMap (vZaF, vZaF)) == 99999990000000); *)
    assert (.~(flat_map_take         (vHi,  vLo))  == 405000000);
    (* assert (.~(zip_filter_filter     (v,    vHi))  == 64000000);
    assert (.~(zip_flat_flat         (v,    vLo))  == 3250000000);
    assert (.~(decoding              (v,    v))    == 100000000); *)
    print_endline "All done"
   >.
end

module M = Benchmark_iter

let main () =
  let compiler = "ocamlfind ocamlopt -O2 -unsafe -nodynlink -package iter -linkpkg util.cmx" in 
  match Sys.argv with
  | [|_;"test"|] ->
    Benchmark.run_natively M.test
      ~compiler
      (* ~save:true *)
  | _ ->
    Benchmark.run_script M.script
      ~compiler

let _ = main ()
