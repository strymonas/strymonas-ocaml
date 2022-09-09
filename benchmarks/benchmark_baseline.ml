module Benchmark_baseline = struct
  open Benchmark_types
  open Benchmark

let sum : int array code -> int code
= fun arr1' ->
  .< let arr1 = .~arr1' in
    let arr1_len = Array.length arr1 in
    let sum = ref 0 in
    for counter1 = 0 to arr1_len - 1 do
      sum := !sum + arr1.(counter1);
    done;
    !sum >.

let sumOfSquares : int array code -> int code
= fun arr1' ->
  .< let arr1 = .~arr1' in
    let arr1_len = Array.length arr1 in
    let sum = ref 0 in
    for counter1 = 0 to arr1_len - 1 do
      let item1 = arr1.(counter1) in
      sum := !sum + item1*item1;
    done;
    !sum >.

let maps : int array code -> int code
= fun arr1' ->
  .< let arr1 = .~arr1' in
    let arr1_len = Array.length arr1 in
    let sum = ref 0 in
    for counter1 = 0 to arr1_len - 1 do
      let item1 = arr1.(counter1) in
      sum := !sum + item1*1*2*3*4*5*6*7;
    done;
    !sum >.;;

let filters : int array code -> int code
= fun arr1' ->
.<let arr1 = .~arr1' in
  let arr1_len = Array.length arr1 in
  let sum = ref 0 in
    for counter1 = 0 to arr1_len - 1 do
    let item1 = arr1.(counter1) in
    if (item1 > 1 && item1 > 2 && item1 > 3 &&
        item1 > 4 && item1 > 5 && item1 > 6 &&
        item1 > 7) then
    begin
      sum := !sum + item1;
    end;
    done;
    !sum>.

let sumOfSquaresEven : int array code -> int code
= fun arr1' ->
  .<let arr1 = .~arr1' in
    let arr1_len = Array.length arr1 in
    let sum = ref 0 in
    for counter1 = 0 to arr1_len - 1 do
      let item1 = arr1.(counter1) in
      if item1 mod 2 = 0 then
      begin
        sum := !sum + item1*item1
      end;
    done;
    !sum>.;;

let cart : (int array code * int array code) -> int code
= fun (arr1', arr2') ->
    .< let arr1 = .~arr1' in
        let arr1_len = Array.length arr1 in
        let arr2 = .~arr2' in
        let arr2_len = Array.length arr2 in
        let sum = ref 0 in
        for counter1 = 0 to arr1_len - 1 do
          let item1 = arr1.(counter1) in
          for counter2 = 0 to arr2_len - 1 do
              let item2 = arr2.(counter2) in
              sum := !sum + item1 * item2;
          done;
        done;
        !sum >.;;

(* let cart : (int array code * int array code) -> int code
= fun (arr1', arr2') ->
    .< let arr1 = .~arr1' in
        let arr1_last = Array.length arr1 - 1 in
        let arr2 = .~arr2' in
        let arr2_last = Array.length arr2 - 1 in
        let sum = ref 0 in
        for counter1 = 0 to arr1_last do
          let item1 = arr1.(counter1) in
          for counter2 = 0 to arr2_last do
              let item2 = arr2.(counter2) in
              sum := !sum + item1 * item2;
          done;
        done;
        !sum >.;; *)

let dotProduct : (int array code * int array code) -> int code
= fun (arr1', arr2') ->
  .< let arr1 = .~arr1' in
    let arr1_len = Array.length arr1 in
    let arr2 = .~arr2' in
    let arr2_len = Array.length arr2 in
    let sum = ref 0 in
    let len = min arr1_len arr2_len in
    for counter = 0 to  len - 1 do
      let item1 = arr1.(counter) in
      let item2 = arr2.(counter) in
      sum := !sum + item1 * item2;
    done;
    !sum >.;;

let flatMap_after_zipWith : (int array code * int array code) -> int code
= fun (arr1', arr2') ->
  .< let arr1 = .~arr1' in
    let arr1_len = Array.length arr1 in
    let arr2 = .~arr2' in
    let arr2_len = Array.length arr2 in
    let sum = ref 0 in
    for counter1 = 0 to arr1_len - 1 do
        let x = arr1.(counter1) + arr1.(counter1) in
        for counter2 = 0 to arr2_len - 1 do
        let item2 = arr2.(counter2) in
          sum := !sum + item2 + x;
        done;
    done;
    !sum>.

let zipWith_after_flatMap : (int array code * int array code) -> int code
= fun (arr1', arr2') ->
  .< let arr1 = .~arr1' in
    let arr1_len = Array.length arr1 in
    let arr2 = .~arr2' in
    let arr2_len = Array.length arr2 in
    let sum = ref 0 in
    let index1 = ref 0 in
    let index2 = ref 0 in
    let flag1 = ref ((!index1) <= (arr1_len - 1)) in
    while (!flag1) && ((!index2) <= (arr2_len - 1)) do
        let el2 = arr2.(!index2) in
        incr index2;
        (let index_zip = ref 0 in
        while (!flag1) && ((!index_zip) <= (arr1_len - 1)) do
          let el1 = arr1.(!index_zip) in
          incr index_zip;
          let elz = arr1.(!index1) in
          incr index1;
          flag1 := ((!index1) <= (arr1_len - 1));
          sum := ((!sum) + (elz + el1 + el2))
          done)
        done;
    !sum >.

let flat_map_take : (int array code * int array code) -> int code
= fun (arr1', arr2') ->
.< let arr1 = .~arr1' in
    let arr2 = .~arr2' in
    let counter1 = ref 0 in
    let counter2 = ref 0 in
    let sum = ref 0 in
    let n = ref 0 in
    let flag = ref true in
    let size1 = Array.length arr1 in
    let size2 = Array.length arr2 in
    while !counter1 < size1 && !flag do
      let item1 = arr1.(!counter1) in
        while !counter2 < size2 && !flag do
          let item2 = arr2.(!counter2) in
          sum := !sum + item1 * item2;
          counter2 := !counter2 + 1;
          n := !n + 1;
          if !n = 20000000 then
          flag := false
        done;
      counter2 := 0;
      counter1 := !counter1 + 1;
    done;
    !sum >.

let zip_filter_filter :(int array code * int array code) -> int code
  = fun (arr1, arr2) ->
  .< let arr1 = .~arr1 in
      let arr2 = .~arr2 in
      let counter1 = ref 0 in
      let counter2 = ref 0 in
      let sum = ref 0 in
      let size1 = Array.length arr1 in
      let size2 = Array.length arr2 in
      while !counter1 < size1 && !counter2 < size2 do
        while not (arr1.(!counter1) > 7 && !counter1 < size1) do
          incr counter1
        done;
        if !counter1 < size1 then begin
        let item2 = arr2.(!counter2) in
        if item2 > 5 then begin
          sum := !sum + arr1.(!counter1) + item2; 
          incr counter1
        end;
        incr counter2
      end
  done;
  !sum >.

(* The following code is original and has a mistake! Compare it with our codes.
def zip_flat_flat_baseline(): Int = {
  val vHi = v
  var sum = 0
  var index11 = 0
  var index12 = 0
  var index21 = 0
  var index22 = 0
  var taken = 0
  val toTake = 20000000
  while (index11 < vHi.length && taken < toTake) {
    index12 = 0
    while (index12 < vLo.length && taken < toTake) {
      var el1 = vHi(index11) * vLo(index12)
      if (index22 > vHi.length) {
        index21 += 1
        index22 = 0
      }
      if (index21 < vLo.length && index22 < vHi.length) {
        sum += el1 + vLo(index21) * vHi(index22)
        taken += 1
      }
      index12 += 1
    }
    index11 += 1
  }
  sum
}
*)
let zip_flat_flat :(int array code * int array code) -> int code
  = fun (arr1, arr2) ->
  .<  let arr1 = .~arr1 in
      let arr2 = .~arr2 in
      let index11 = ref 0 in
      let index12 = ref 0 in
      let index21 = ref 0 in
      let index22 = ref 0 in
      let sum = ref 0 in
      let taken = ref 0 in
      let toTake = 200_000_000 in
      let size1 = Array.length arr1 in
      let size2 = Array.length arr2 in
      let goon = ref true in
      while !index11 < size1 && !taken < toTake && !goon do
        index12 := 0;
        while !index12 < size2 && !taken < toTake && !goon do
          let el1 = arr1.(!index11) * arr2.(!index12) in
          if !index22 >= size1 then begin 
            incr index21; 
            index22 := 0
          end;
          if !index21 >= size2 then goon := false else begin
            if !index22 < size1 then begin
              sum := !sum + el1 + arr2.(!index21) - arr1.(!index22);
              incr taken;
              incr index22
            end
          end;
          incr index12
        done;
        incr index11
      done;
    !sum
  >.

let decoding : int array code * int array code -> int code =
  fun (arr1, arr2) -> .<
  let arr1 = .~arr1 in
  let arr2 = .~arr2 in
  let byte_max = 255 in
  let guard = ref true in
  let sum = ref 0 in
  let j1  = ref 0 in
  let j2  = ref 0 in
  let i1  = ref 0 in
  while (!i1 < Array.length arr1 && !guard) do
    let t1 = arr1.(!i1) in
    let miner = if (t1<byte_max-1)
                then t1
                else byte_max-1 in 
    let i2  = ref 0 in
    while (!i2 <= miner && !guard) do
      let l = !i2>=t1 in
      let t2 = arr2.(!j1) in
      if (!j2<byte_max) then
        let r = !j2>=t2 in
        if (l||r) then begin
          incr sum;
        end;
        incr j2;
        if (!j2>t2) then begin
          incr j1;
          if (!j1>=Array.length arr2) then guard:=false;
          j2:=0
        end;
        incr i2
      else begin
        incr j1;
        if (!j1>=Array.length arr2) then guard:=false;
        j2:=0
      end;
    done;
    incr i1
  done;
  !sum >.


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
     perfS  "sum_baseline" v sum options;
     perfS  "sumOfSquares_baseline" v sumOfSquares options;
     perfS  "sumOfSquaresEven_baseline" v sumOfSquaresEven options;
     perfS  "mapsMegamorphic_baseline" v maps options;
     perfS  "filtersMegamorphic_baseline" v filters options;
     perfS2 "cart_baseline" vHi vLo cart options;
     perfS2 "dotProduct_baseline" vHi vHi dotProduct options;
     perfS2 "flatMapAfterZip_baseline" vFaZ vFaZ
      flatMap_after_zipWith options;
     perfS2 "zipAfterFlatMap_baseline" vZaF vZaF 
      zipWith_after_flatMap options;
     perfS2 "flatMapTake_baseline" vHi vLo
      flat_map_take options;
     perfS2 "zipFilterFilter_baseline" v vHi
      zip_filter_filter options;
     perfS2 "zipFlatMapFlatMap_baseline" v vLo
      zip_flat_flat options;
     perfS2 "runLengthDecoding_baseline" v v
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
end

module M = Benchmark_baseline

let main () =
  match Sys.argv with
  | [|_;"test"|] ->
    Benchmark.run_natively M.test
      (* ~save:true *)
  | _ ->
    Benchmark.run_script M.script

let _ = main ()
