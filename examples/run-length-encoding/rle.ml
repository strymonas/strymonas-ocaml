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
      Raw.pull_array el @@ fun i k ->
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
    let t_2 = (Stdlib.Array.length arg1_1) - 1 in
    let v_3 = Stdlib.ref [] in
    (let v_4 = Stdlib.ref 0 in
     for i_5 = 0 to t_2 do
       let el_6 = Stdlib.Array.get arg1_1 i_5 in
       let t_7 = el_6 > 0 in
       let t_8 = ! v_4 in
       if t_7
       then (v_4 := 0; v_3 := (t_8 :: (! v_3)))
       else
         (v_4 := (t_8 + 1);
          if (! v_4) = 255 then (v_4 := 0; v_3 := (255 :: (! v_3))))
     done);
    ! v_3>.
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
  fun arg1_15 ->
    let t_16 = (Stdlib.Array.length arg1_15) - 1 in
    let v_17 = Stdlib.ref [] in
    (let v_18 = Stdlib.ref 0 in
     for i_19 = 0 to t_16 do
       let el_20 = Stdlib.Array.get arg1_15 i_19 in
       for i_21 = 0 to el_20 do
         if i_21 < el_20
         then
           let t_23 = ! v_18 in
           (v_18 := (t_23 + 1);
            if (! v_18) = 255 then (v_18 := 0; v_17 := (255 :: (! v_17))))
         else
           if i_21 < 255
           then (let t_22 = ! v_18 in v_18 := 0; v_17 := (t_22 :: (! v_17)))
       done
     done);
    ! v_17>.
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
  fun (arg1_24, arg2_25) ->
    let t_26 = (Stdlib.Array.length arg2_25) - 1 in
    let t_27 = (Stdlib.Array.length arg1_24) - 1 in
    let v_30 = Stdlib.ref 0 in
    (let v_31 = Stdlib.ref 0 in
     let v_33 = Stdlib.ref true in
     let v_34 = Stdlib.ref false in
     let v_35 = Stdlib.ref (Stdlib.Obj.obj (Stdlib.Obj.new_block 0 0)) in
     let v_36 = Stdlib.ref 0 in
     let i_37 = Stdlib.ref 0 in
     while ((! i_37) <= t_27) && (! v_33) do
       let iv_38 = ! i_37 in
       i_37 := (iv_38 + 1);
       (let el_39 = Stdlib.Array.get arg1_24 iv_38 in
        let i_40 = Stdlib.ref 0 in
        while ((! i_40) <= el_39) && (! v_33) do
          let iv_41 = ! i_40 in
          i_40 := (iv_41 + 1);
          if iv_41 < el_39
          then
            (let v_44 = Stdlib.ref true in
             while ! v_44 do
               if Stdlib.not (! v_34)
               then
                 (if (! v_31) <= t_26
                  then
                    ((let el_45 = Stdlib.Array.get arg2_25 (! v_31) in
                      v_35 := el_45;
                      v_36 := 0;
                      v_34 := ((! v_36) <= (! v_35)));
                     Stdlib.incr v_31)
                  else (v_33 := false; v_44 := false));
               if ! v_34
               then
                 ((if (! v_36) < (! v_35)
                   then (v_30 := (! v_30); v_44 := false)
                   else
                     if (! v_36) < 255
                     then (v_30 := ((! v_30) + 1); v_44 := false);
                   Stdlib.incr v_36);
                  v_34 := ((! v_36) <= (! v_35)))
               done)
          else
            if iv_41 < 255
            then
              (let v_42 = Stdlib.ref true in
               while ! v_42 do
                 if Stdlib.not (! v_34)
                 then
                   (if (! v_31) <= t_26
                    then
                      ((let el_43 = Stdlib.Array.get arg2_25 (! v_31) in
                        v_35 := el_43;
                        v_36 := 0;
                        v_34 := ((! v_36) <= (! v_35)));
                       Stdlib.incr v_31)
                    else (v_33 := false; v_42 := false));
                 if ! v_34
                 then
                   ((if (! v_36) < (! v_35)
                     then (v_30 := ((! v_30) + 1); v_42 := false)
                     else
                       if (! v_36) < 255
                       then (v_30 := ((! v_30) + 1); v_42 := false);
                     Stdlib.incr v_36);
                    v_34 := ((! v_36) <= (! v_35)))
                 done)
          done)
       done);
    ! v_30>.
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
