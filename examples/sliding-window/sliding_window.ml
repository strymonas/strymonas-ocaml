(* Sliding window example:
   computing sliding average

  Example of using the raw-level API

  We also demonstrate using abstractions (records and first-class
  functions) and *not* paying for them.
  We implement two transformers: one is a general-purpose sliding window
  transformer, converting a t cstream to a t sliding_window stream.
  The second transformer is an aggregation function over sliding window,
  namely, averaging. We can likewise write many similar aggregation functions.

*)
(*
#directory "../../lib";;
#directory "../../lib/backends/Trx";;
#directory "../../lib/backends/C";;
#load "stream.cma";;
*)

(* The identity function that makes CPS convenient *)
let (let-) c k = c k

module type cde_ex     = module type of Cde_ex
module type stream_raw = module type of Stream_raw
module type stream     = module type of Stream_cooked


(* ceil_power2 n is the least such m that m is an exact power of 2 and
   n <= m
*)
let ceil_power2 : int -> int = fun n ->
   let rec loop m = if m >= n then m else loop (2*m) in
   loop 1

module Make(C:cde_ex) = struct
  open Stream_cooked_fn.Make(C)

 (* A rather-general purpose transformer, from
    t cstream to t sliding_window stream. 
    The first argument is the window size; the second is the initializer
    (the filler; only its type is actually important).
 *)
 type 'a sliding_window =
    {size: int;              (* non-negative *)
     elem: 'a C.exp;         (* the current stream element, just deposited 
                                into the window *)
     elem_index: int C.exp;  (* The current element count since the beginning
                                of the stream. The very first element has
                                elem_index = 0.
                                If elem_index < size-1, the window is not
                                yet filled.
                                *)
     (* the function to get earlier stream elements,
        earlier than elem, from the window *)
     elem_get: int C.exp -> ('a C.exp -> unit C.stm) -> unit C.stm; 
   }

 let sliding_window : int -> 'a C.tbase -> 'a cstream -> 
   'a sliding_window stream = fun size ty st ->
  assert (size>0);
  let buffer_len = ceil_power2 size in
  let mask = C.int (buffer_len - 1) in
  let- buffer = Raw.initializing_uarr ty buffer_len in
  Raw.zip_raw st (iota C.(int 0)) 
  |> Raw.map_raw  (fun (v,i) k ->
   let open C in
   array_set buffer (logand i mask) v @.
   k {size; elem=v; elem_index=i; 
      elem_get=(fun i k -> array_get buffer (logand i mask) k)}
  )

 (* This is a sample aggregator over the sliding window: sliding average
   Until the window is filled, no averages are produced.
 *)
 let sliding_average : int sliding_window stream -> C.F64.t cstream = fun st ->
  let- sliding_sum = Raw.initializing_ref C.(int 0) in
  st |> Raw.map_raw ~linear:false (fun {size;elem;elem_index;elem_get} k ->
    let open C in
    if_ (elem_index < int Stdlib.(size-1))
        (* filling-in the window *)
        begin
         sliding_sum := dref sliding_sum + elem
        end
        (* really sliding *)
        begin
          let- curr_sum = letl (dref sliding_sum + elem) in
          let- res = 
            letl F64.(of_int curr_sum /. lit Stdlib.(float_of_int size)) in
          let- old = elem_get (elem_index - int Stdlib.(size-1)) in
          seq (sliding_sum := curr_sum - old) @@ k res
        end
 )

end

(* The OCaml-generation backend, with extension suppporting OCaml lists *)
module C = Backends.MetaOCamlExt

let test1 = 
  let open Stream_cooked_fn.Make(C) in
  let open Make(C) in
  of_int_array [|1;2;3;4;5;6;7|] 
  |> sliding_window 2 C.tint 
  |> sliding_average 
  |> fold (Fun.flip C.cons) (C.nil ())

(*
val test1 : float list C.stm =
  C.Stm .<
   let t_4 = [|1;2;3;4;5;6;7|] in
   let t_3 = [|0;0|] in
   let v_1 = Stdlib.ref [] in
   (let v_2 = Stdlib.ref 0 in
    let v_5 = Stdlib.ref 0 in
    for i_6 = 0 to 7 - 1 do
      let el_7 = Stdlib.Array.get t_4 i_6 in
      let t_8 = ! v_5 in
      Stdlib.incr v_5;
      Stdlib.Array.set t_3 (Stdlib.Int.logand t_8 1) el_7;
      if t_8 < 1
      then v_2 := ((! v_2) + el_7)
      else
        (let t_9 = (! v_2) + el_7 in
         let t_10 = (Stdlib.Float.of_int t_9) /. 2. in
         let el_11 = Stdlib.Array.get t_3 (Stdlib.Int.logand (t_8 - 1) 1) in
         v_2 := (t_9 - el_11); v_1 := (t_10 :: (! v_1)))
    done);
   ! v_1>. 
*)


let[@warning "-8"] 
    [1.5; 2.5; 3.5; 4.5; 5.5; 6.5] = 
  test1 |> C.to_code |> Runcode.run |> List.rev

let test2 = 
  let open Stream_cooked_fn.Make(C) in
  let open Make(C) in
  of_int_array [|1;2;3;4;5;6;7|]
  |> sliding_window 3 C.tint
  |> sliding_average 
  |> fold (Fun.flip C.cons) (C.nil ())

let[@warning "-8"] 
    [2.; 3.; 4.; 5.; 6.] = 
  test2 |> C.to_code |> Runcode.run |> List.rev


(* We can use these transformers as all others, with the guarantee
   of perfect fusion
 *)
let test3 = 
  let open Stream_cooked_fn.Make(C) in
  let open Make(C) in
  zip_with C.pair 
   (iota C.(int 1))
   (of_int_array [|1;2;3;4;5;6;7;8;9|]
      |> filter C.(fun x -> x mod (int 2) = int 1) 
      |> sliding_window 3 C.tint 
      |> sliding_average
      |> map C.F64.(fun x -> x *. lit 3.)) 
  |> fold (Fun.flip C.cons) (C.nil ())

let[@warning "-8"] 
    [(1, 9.); (2, 15.); (3, 21.)] = 
  test3 |> C.to_code |> Runcode.run |> List.rev


module Test4(C:cde_ex) = struct
  open Stream_cooked_fn.Make(C)
  open Make(C)
  let test4 = 
    zip_with C.(fun x y -> int 100 * x + y)
      (iota C.(int 1))
      (of_int_array [|1;2;3;4;5;6;7;8;9|]
       |> filter C.(fun x -> x mod (int 2) = int 1) 
       |> sliding_window 3 C.tint
       |> sliding_average
       |> map C.F64.truncate) 
    |> iter C.print_int
end

let _ = 
  let module M = Test4(C) in M.test4

(*
- : unit C.stm =
C.Stm .<
 let t_40 = [|1;2;3;4;5;6;7;8;9|] in
 let t_39 = [|0;0;0;0|] in
 let v_37 = Stdlib.ref 1 in
 let v_38 = Stdlib.ref 0 in
 let v_41 = Stdlib.ref 0 in
 for i_42 = 0 to 9 - 1 do
   let el_43 = Stdlib.Array.get t_40 i_42 in
   if (el_43 mod 2) = 1
   then
     let t_44 = ! v_41 in
     (Stdlib.incr v_41;
      Stdlib.Array.set t_39 (Stdlib.Int.logand t_44 3) el_43;
      if t_44 < 2
      then v_38 := ((! v_38) + el_43)
      else
        (let t_45 = (! v_38) + el_43 in
         let t_46 = (Stdlib.Float.of_int t_45) /. 3. in
         let el_47 = Stdlib.Array.get t_39 (Stdlib.Int.logand (t_44 - 2) 3) in
         v_38 := (t_45 - el_47);
         (let t_48 = Stdlib.truncate t_46 in
          let t_49 = ! v_37 in
          Stdlib.incr v_37;
          Stdlib.Format.print_int ((100 * t_49) + t_48);
          Stdlib.Format.force_newline ())))
 done>. 
*)

let () = 
  let module M = Test4(C) in (Runcode.run @@ C.to_code M.test4;
                              Format.print_flush ())
(*
103
205
307
*)

(* Now try C code generation *)
module CC = Backends.C

let _ = let module M = Test4(CC) in 
  CC.print_code M.test4

(*
void fn(){
  int x_1 = 1;
  int x_2 = 0;
  int a_3[4];
  static int a_4[9] = {1,2,3,4,5,6,7,8,9};;
  int x_5 = 0;
  for (int i_6 = 0; i_6 < 9; i_6 += 1){
    int const t_7 = a_4[i_6];
    if ((t_7 % 2) == 1)
    {
      int const t_8 = x_5;
      x_5++;
      (a_3[t_8 & 3]) = t_7;
      if (t_8 < 2)
      
        x_2 = x_2 + t_7;
      else {
        int const t_9 = x_2 + t_7;
        double const t_10 = ((double)t_9) / 3.;
        int const t_11 = a_3[(t_8 - 2) & 3];
        x_2 = t_9 - t_11;
        int const t_12 = (int)t_10;
        int const t_13 = x_1;
        x_1++;
        printf("%d\n",(100 * t_13) + t_12);
      }
    }
  }
}
*)

(* Run the generated C code, capture its output as a string and compare
   with the expected output
*)
let[@warning "-8"] "103\n205\n307\n" = 
  let module M = Test4(CC) in 
  CC.run_capture_output M.test4 |> fun str -> Scanf.bscanf str "%s@\000" Fun.id 

let () = print_endline "All done"

;;

          
