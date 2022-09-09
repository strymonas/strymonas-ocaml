module type cde = sig
  include module type of Cde_ex
  val to_code1 : ('a cde -> 'b cde) -> ('a code -> 'b code)
  val to_code2 : ('a cde * 'b cde -> 'c cde) -> ('a code * 'b code -> 'c code)
end

module Benchmark_stream(C:cde) = struct
  module S = struct 
    include Stream_cooked_fn.Make(C)
    type 'a stream = 'a cstream

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
        Raw.pull_array el @@ fun i k ->
          let open C in
          if_ (i < el) (k (bool false))
              (if1 (i < int byte_max) (k (bool true)))
        )
  end

  open S
  open Benchmark_types
  open Benchmark
  open Benchmark_abstract.Benchmark(C)(S)


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
      perfS  "sum_staged" v sum options;
      perfS  "sumOfSquares_staged" v sumOfSquares options;
      perfS  "sumOfSquaresEven_staged" v sumOfSquaresEven options;
      perfS  "mapsMegamorphic_staged" v maps options;
      perfS  "filtersMegamorphic_staged" v filters options;
      perfS2 "cart_staged" vHi vLo cart	options;
      perfS2 "dotProduct_staged" vHi vHi dotProduct options;
      perfS2 "flatMapAfterZip_staged" vFaZ vFaZ 
       flatMap_after_zipWith options;
      perfS2 "zipAfterFlatMap_staged" vZaF vZaF 
       zipWith_after_flatMap options;
      perfS2 "flatMapTake_staged" vHi vLo
       flat_map_take options;
      perfS2 "zipFilterFilter_staged" v vHi
       zip_filter_filter options;
      perfS2 "zipFlatMapFlatMap_staged" v vLo
       zip_flat_flat options;
      perfS2 "runLengthDecoding_staged" v v
       decoding options;
   |];;

   let test = .<
    print_endline "Last checked: Jun 2, 2022";
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
end;;

(*
module M = Benchmark_stream(Benchmark_abstract.CodeBasic)
*)
module M = Benchmark_stream(Benchmark_abstract.CodePV)

let main () =
  match Sys.argv with
  | [|_;"test"|] ->
    Benchmark.run_natively M.test
      (* ~save:true *)
  | _ ->
    Benchmark.run_script M.script

let _ = main ()
