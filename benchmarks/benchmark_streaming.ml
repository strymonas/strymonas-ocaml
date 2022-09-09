(* 
#require "streaming";;
open Streaming;;
let st = Source.array [|1;2;3;4;5;6;7;8;9;10|] in
Stream.from (Source.zip st st)
|> Stream.fold (fun (z1,z2) (x1,x2) -> (z1+x1, z2+x2) ) (0,0)

- https://odis-labs.github.io/streaming/streaming/index.html#what's-the-difference-between-sources-and-streams?
"In general, streams offer better performance than sources for
the most common operations (including concatenation) and offer integration with
sinks and flows. On the other hand, sources are easier to create, and support zipping."

- https://odis-labs.github.io/streaming/streaming/Streaming/Stream/index.html
"Streams are built to be compatible with sources, sinks and flows.
To create a stream that produces all elements from a source use
Stream.from, to consume a stream with a sink use Stream.into and
to transform stream elements with a flow use Stream.via.
For more sophisticated pipelines that might have source leftovers,
run can be used."

In conclusion, streaming cannot zip a nested stream.
*)

module Streaming_intf = struct  
  open Streaming.Stream
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
  let decode = fun st -> failwith "unusable"
end


module Benchmark_streaming = struct
  open Benchmark_types
  open Benchmark

  open Streaming_intf

  module C = Benchmark_abstract.CodeBasic
  open Benchmark_abstract.Benchmark(C)(Streaming_intf)
  
  open Streaming
  let of_arr arr = .<Source.array .~arr>.
  let map f st = .<Stream.from (Source.map (fun x -> .~(f .<x>.)) .~st)>.
  let filter f st = .<Source.filter (fun x -> .~(f .<x>.)) .~st>.
  let zip_with f st1 st2 =
    .<Stream.from (Source.zip_with (fun x y -> .~(f .<x>. .<y>.)) .~st1 .~st2)>.

  let dotProduct : (int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    zip_with C.( * ) (of_arr arr1) (of_arr arr2)
    |> sum_int

  let flatMap_after_zipWith : (int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    zip_with C.( + ) (of_arr arr1) (of_arr arr1)
    |> flat_map (fun x -> of_arr arr2 |> map C.(fun el -> el + x))
    |> sum_int

  let zip_filter_filter :(int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    zip_with C.( + )
      (of_arr arr1 |> filter C.(fun x -> x > int 7))
      (of_arr arr2 |> filter C.(fun x -> x > int 5))
    |> sum_int


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
      perfS  "sum_streaming" v sum options;
      perfS  "sumOfSquares_streaming" v sumOfSquares options;
      perfS  "sumOfSquaresEven_streaming" v sumOfSquaresEven options;
      perfS  "mapsMegamorphic_streaming" v maps options;
      perfS  "filtersMegamorphic_streaming" v filters options;
      perfS2 "cart_streaming" vHi vLo cart options;
      perfS2 "dotProduct_streaming" vHi vHi dotProduct options;
      perfS2 "flatMapAfterZip_streaming" vFaZ vFaZ 
       flatMap_after_zipWith options;
      (* perfS2 "zipAfterFlatMap_streaming" vZaF vZaF 
       zipWith_after_flatMap options; *)
      perfS2 "flatMapTake_streaming" vHi vLo
        flat_map_take options;
      perfS2 "zipFilterFilter_streaming" v vHi
        zip_filter_filter options;
      (* perfS2 "zipFlatMapFlatMap_streaming" v vLo
        zip_flat_flat options; *)
      (* perfS2 "runLengthDecoding_streaming" v v
        decoding options; *)
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
    (* assert (.~(zipWith_after_flatMap (vZaF, vZaF)) == 99999990000000); *)
    assert (.~(flat_map_take         (vHi,  vLo))  == 405000000);
    assert (.~(zip_filter_filter     (v,    vHi))  == 64000000);
    (* assert (.~(zip_flat_flat         (v,    vLo))  == 3250000000); *)
    (* assert (.~(decoding              (v,    v))    == 100000000); *)
    print_endline "All done"
   >.
end;;

module M = Benchmark_streaming

let main () =
  let compiler = "ocamlfind ocamlopt -O2 -unsafe -nodynlink -package streaming -linkpkg util.cmx" in 
  match Sys.argv with
  | [|_;"test"|] ->
    Benchmark.run_natively M.test
      ~compiler
      (* ~save:true *)
  | _ ->
    Benchmark.run_script M.script
      ~compiler

let _ = main ()
