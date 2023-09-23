(* Run-Length Encodings

  Example of using the raw-level API and realistic and complicated
  flat-mapping

  The source is a boolean stream, often with long stretches of zeros
  (i.e., false). The encoded stream is a stream of
  8-bit unsigned integers: An element 0<=n<255 represents a stretch of
  n zeros/false followed by one (true) in the original stream.
  An element 255 represented a stretch of 255 zeros in the original,
  NOT followed by one.

  This is not just a realistic example: it is very real. Back in 1990 I have
  implemented something very close when automating a neurophysiological
  experiment. The boolean stream is the stream of neuron firings: 
  true means a neuron firing. The encoding was done on the computer
  acquiring signals from neurons. The decoding was done in a MEX Matlab
  module I wrote (to correlate signals in Matlab).

*)

(*
#directory "../../lib";;
#directory "../../lib/backends/Trx";;
#directory "../../lib/backends/C";;
#load "stream.cma";;
*)

module type cde_ex     = module type of Cde_ex

module Make(C:cde_ex) = struct
  open Stream_cooked_fn.Make(C)

  (* XXX For C, the byte should be uint8_t. *)
  type byte = int                        (* element of an outer stream *)
  let byte_max = 255

  let encode : bool cstream -> byte cstream = fun st ->
    Raw.initializing_ref C.(int 0) @@ fun zeros_count ->
    st |> Raw.map_raw ~linear:false (fun el k ->
      let open C in
      letl (dref zeros_count) @@ fun zeros ->
      if_ el (
        (zeros_count := int 0) @.
        k zeros
      ) (
        (zeros_count := zeros + int 1) @.
        if1 (dref zeros_count = int byte_max) (
          (zeros_count := int 0) @.
          k (int byte_max)
        )
      )
    )

  (* advanced and more interesting use of flat_map *)
  let decode : byte cstream -> bool cstream =
    flat_map @@ fun el ->
      Raw.pull_array C.(el+ int 1) @@ fun i k ->
        let open C in
        if_ (i < el) (
          k (bool false)
        ) @@
        if1 (i < int byte_max) (
          k (bool true)
        )
end

module C = Backends.MetaOCamlExt

let test1 = 
  let open Stream_cooked_fn.Make(C) in
  let open Make(C) in
  C.one_arg_fun @@ fun arr ->
    of_arr arr 
    |> map C.(fun x -> x > int 0)
    |> encode
    |> fold (Fun.flip C.cons) (C.nil ())
(*
val test1 : (int array -> int list) code = .<
  fun arg1_1 ->
    let v_2 = Stdlib.ref [] in
    (let v_3 = Stdlib.ref 0 in
     for i_4 = 0 to (Stdlib.Array.length arg1_1) - 1 do
       let el_5 = Stdlib.Array.get arg1_1 i_4 in
       let t_6 = el_5 > 0 in
       let t_7 = ! v_3 in
       if t_6
       then (v_3 := 0; v_2 := (t_7 :: (! v_2)))
       else
         (v_3 := (t_7 + 1);
          if (! v_3) = 255 then (v_3 := 0; v_2 := (255 :: (! v_2))))
     done);
    ! v_2>.
*)

let[@warning "-8"] [0; 0; 1; 2]
 = List.rev @@ Runcode.run test1 [|1;1;0;1;0;0;1|]

let test11 = 
  let open Stream_cooked_fn.Make(C) in
  let open Make(C) in
  C.one_arg_fun @@ fun arr ->
    of_arr arr 
    |> decode
    |> map C.(fun x -> cond x (int 1) (int 0))
    |> fold (Fun.flip C.cons) (C.nil ())

let t11 = [0;1;1] @ (List.init 255 (fun i -> 0)) @ [0;1]
let _ = assert (t11 = List.rev @@ Runcode.run test11 [|1;0;255;1|])

let test2 = 
  let open Stream_cooked_fn.Make(C) in
  let open Make(C) in
  C.one_arg_fun @@ fun arr ->
    of_arr arr 
    |> decode
    |> encode
    |> fold (Fun.flip C.cons) (C.nil ())

(*
val test2 : (int array -> int list) code = .<
  fun arg1_13 ->
    let v_14 = Stdlib.ref [] in
    (let v_15 = Stdlib.ref 0 in
     for i_16 = 0 to (Stdlib.Array.length arg1_13) - 1 do
       let el_17 = Stdlib.Array.get arg1_13 i_16 in
       for i_18 = 0 to (el_17 + 1) - 1 do
         if i_18 < el_17
         then
           let t_20 = ! v_15 in
           (v_15 := (t_20 + 1);
            if (! v_15) = 255 then (v_15 := 0; v_14 := (255 :: (! v_14))))
         else
           if i_18 < 255
           then (let t_19 = ! v_15 in v_15 := 0; v_14 := (t_19 :: (! v_14)))
       done
     done);
    ! v_14>.
*)

let[@warning "-8"] true =
   let x = [0;1;2;3;4;0;1;255;0;255;255;0] in
   x =
   (x |> Array.of_list |> Runcode.run test2 |> List.rev)

let[@warning "-8"] true =
   let x = List.init 400 (fun i -> i mod 256) in
   x =
   (x |> Array.of_list |> Runcode.run test2 |> List.rev)



let bench =
  let open Stream_cooked_fn.Make(C) in
  let open Make(C) in
  C.two_arg_fun @@ fun (arr1,arr2) ->
    zip_with C.(||) (of_arr arr1 |> decode) (of_arr arr2 |> decode)
    |> map C.(fun x -> cond x (int 1) (int 0))
    |> sum_int
(*
val bench : (int array * int array -> int) code = .<
  fun (arg1_21, arg2_22) ->
    let v_25 = Stdlib.ref 0 in
    (let v_26 = Stdlib.ref 0 in
     let v_28 = Stdlib.ref true in
     let v_29 = Stdlib.ref false in
     let v_30 = Stdlib.ref (Stdlib.Obj.obj (Stdlib.Obj.new_block 0 0)) in
     let v_31 = Stdlib.ref 0 in
     let i_32 = Stdlib.ref 0 in
     while ((! i_32) < (Stdlib.Array.length arg1_21)) && (! v_28) do
       let iv_33 = ! i_32 in
       i_32 := (iv_33 + 1);
       (let el_34 = Stdlib.Array.get arg1_21 iv_33 in
        let i_35 = Stdlib.ref 0 in
        while ((! i_35) < (el_34 + 1)) && (! v_28) do
          let iv_36 = ! i_35 in
          i_35 := (iv_36 + 1);
          if iv_36 < el_34
          then
            (let v_39 = Stdlib.ref true in
             while ! v_39 do
               if Stdlib.not (! v_29)
               then
                 (if (! v_26) < (Stdlib.Array.length arg2_22)
                  then
                    ((let el_40 = Stdlib.Array.get arg2_22 (! v_26) in
                      v_30 := el_40;
                      v_31 := 0;
                      v_29 := ((! v_31) < ((! v_30) + 1)));
                     Stdlib.incr v_26)
                  else (v_28 := false; v_39 := false));
               if ! v_29
               then
                 ((if (! v_31) < (! v_30)
                   then (v_25 := (! v_25); v_39 := false)
                   else
                     if (! v_31) < 255
                     then (v_25 := ((! v_25) + 1); v_39 := false);
                   Stdlib.incr v_31);
                  v_29 := ((! v_31) < ((! v_30) + 1)))
               done)
          else
            if iv_36 < 255
            then
              (let v_37 = Stdlib.ref true in
               while ! v_37 do
                 if Stdlib.not (! v_29)
                 then
                   (if (! v_26) < (Stdlib.Array.length arg2_22)
                    then
                      ((let el_38 = Stdlib.Array.get arg2_22 (! v_26) in
                        v_30 := el_38;
                        v_31 := 0;
                        v_29 := ((! v_31) < ((! v_30) + 1)));
                       Stdlib.incr v_26)
                    else (v_28 := false; v_37 := false));
                 if ! v_29
                 then
                   ((if (! v_31) < (! v_30)
                     then (v_25 := ((! v_25) + 1); v_37 := false)
                     else
                       if (! v_31) < 255
                       then (v_25 := ((! v_25) + 1); v_37 := false);
                     Stdlib.incr v_31);
                    v_29 := ((! v_31) < ((! v_30) + 1)))
                 done)
          done)
       done);
    ! v_25>.
*)

let[@warning "-8"] 6 =
    let arr1 = [|0;1;255;0;255;1;255;255|] in
    let arr2 = [|1;0;255;0;255;255;0;255;255;255;1|] in
    Runcode.run bench (arr1,arr2)

let bench_hand : int array * int array -> int =
  fun (arr1, arr2) ->
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
  !sum

let[@warning "-8"] 6 =
    let arr1 = [|0;1;255;0;255;1;255;255|] in
    let arr2 = [|1;0;255;0;255;255;0;255;255;255;1|] in
    bench_hand (arr1,arr2);;

(*
The
benchmark then is
        fun st1 st2 -> zip or (decode st1) (decode st2) |> encode
*)

(*
  A related encoding: it is a bit artificial, and also more complex

  1. Let st1 be a stream of non-negative integers, often with long
  stretches of zeros (but not too long: the length can be represented
  in the int data type, for whatever int we use: 16bit, 32bit, etc)

  The encoded stream is the stream of integers, where a negative element
  n represents a stretch of zeros of length |n| in the original stream.

fsm
  0 (el=0) (incr zeros_count) -1
                              1
  1 (zeros_count = 0) (k el) -1
    k zeros_count             2
  2 k el                     -1

        encode : int stream -> int stream
takes a stream and produces a stream in which a run of zeros of length
n is replaced by one element, -n. That is, a positive element stands
for itself, and a negative element n for a sequence of zeros of length
-n. Now consider the pipeline
        fun st1 st2 -> zip max (decode st1) (decode st2) |> encode
where st1 and st2 are encoded streams. Decode is essentially a flat
map. 
*)

let _ = print_endline "\nAll done"
