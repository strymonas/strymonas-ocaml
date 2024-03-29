(* Run-Length Encodings

  Another example of run-length encoding: solving the problem from
  http://okmij.org/ftp/Algorithms/grasping-all-apples-at-once.html

  Specifically, the problem is converting the input like
      "aaaabbbcca"
  to the output like
      [("a",4), ("b", 3), ("c", 2), ("a", 1)]

  The characteristic of the problem is stream with explicit termination,
  and stream look-ahead.

  Since strymonas is made mostly for numeric computations, we slightly
  adjust the example to use integers instead of characters, and
  arrays instead of strings.
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

(* This could have been in the library: and probably will be
   at some point
*)
module TerminatedStream(C:cde_ex) = struct
  open Stream_cooked_fn.Make(C)
  open Raw

  (* A stream with an explicit terminator. The type of the stream,
     'a cde option (statically-visible option) means that we will
      potentially duplicate code: for the regular case and for the
      terminated-stream case.
  *)
  type 'a term_stream = 'a option stream
  
  let of_arr_term : 'a arr -> 'a exp term_stream = 
  fun arr ->
  let- len = initializing C.(array_len arr) in
  let- i   = initializing_ref C.(int 0) in
  infinite C.(fun k ->
    let- iv = letl (dref i) in
    incr i @.
    if_ (iv < len) (
      let- v = array_get arr iv in
      k (Some v)
    ) (
      k None
    )
  )
  |> guard (GExp C.(dref i <= len))
end

module LookAhead(C:cde_ex) = struct
  open Stream_cooked_fn.Make(C)
  open TerminatedStream(C)
  open Raw

  (* A stream with look-ahead: a stream whose elements are tuples
     of the current element and possibly the next element.
     Again, with the statically visible option we opt into code
     duplication and specialization
  *)
  type 'a look_ahead_stream = ('a * 'a option) stream
  
  let look_ahead : 'a tbase -> 'a exp term_stream -> 'a exp look_ahead_stream =
  fun tbase st ->
  let- prev     = initializing_ref C.(tbase_zero tbase) in
  let- saw_prev = initializing_ref C.(bool false) in
  st |> map_raw ~linear:false C.(function
    | Some e -> fun k ->
        if_ (dref saw_prev) (
          let- pv = letl (dref prev) in
          (prev := e) @.
          k (pv, Some e)
        ) (
          (prev := e) @.
          (saw_prev := bool true)
        )
    | None -> fun k ->                  (* stream is terminated *)
        if1 (dref saw_prev) (
          k (dref prev,None)
        )
   )
end

(* Solving the problem, as described on the above page *)
module RLL(C:cde_ex) = struct
  open Stream_cooked_fn.Make(C)
  open TerminatedStream(C)
  open LookAhead(C)

  (* computing group breaks: annotating each element with a boolean:
     true if this element is the last in its group (and so the next
     element (if any) will start a new group)
   *)
  type 'a annot = 'a * bool Raw.exp
  let group : 'a look_ahead_stream -> 'a annot stream = 
    Raw.map_raw' @@ function
          | (x,Some next) -> C.(x, (not (x = next)))
          | (x,_)         -> C.(x,bool true)

  (* counting the group elements *)
  let count : 'a annot stream -> ('a * int Raw.exp) stream = fun st ->
    let- cnt = Raw.initializing_ref C.(int 0) in
    st |> Raw.map_raw ~linear:false @@ fun (x,break) k ->
      let open C in
      if_ break (
        let- cv = letl (dref cnt) in 
        (cnt := int 0) @.
        k (x, cv + int 1)
      ) (
        incr cnt
      )

  let rll arr = arr |> of_arr_term |> look_ahead C.tint |> group |> count

  let rll_print arr = 
      arr |> rll |> 
      iter C.(fun (x,cnt) -> 
        print_int x @. print_int cnt)
end

module CCaml = Backends.MetaOCaml

module M = RLL(CCaml)
let f = CCaml.one_arg_fun M.rll_print

(*
val f : (int array -> unit) code = .<
  fun arg1_9 ->
    let t_10 = Stdlib.Array.length arg1_9 in
    let v_11 = Stdlib.ref 0 in
    let v_12 = Stdlib.ref 0 in
    let v_13 = Stdlib.ref false in
    let v_14 = Stdlib.ref 0 in
    while (! v_14) <= t_10 do
      let t_15 = ! v_14 in
      Stdlib.incr v_14;
      if t_15 < t_10
      then
        (let el_17 = Stdlib.Array.get arg1_9 t_15 in
         if ! v_13
         then
           let t_18 = ! v_12 in
           (v_12 := el_17;
            if Stdlib.not (t_18 = el_17)
            then
              (let t_19 = ! v_11 in
               v_11 := 0;
               (Stdlib.Format.print_int t_18; Stdlib.Format.force_newline ());
               Stdlib.Format.print_int (t_19 + 1);
               Stdlib.Format.force_newline ())
            else Stdlib.incr v_11)
         else (v_12 := el_17; v_13) := true)
      else
        if ! v_13
        then
          (let t_16 = ! v_11 in
           v_11 := 0;
           (Stdlib.Format.print_int (! v_12); Stdlib.Format.force_newline ());
           Stdlib.Format.print_int (t_16 + 1);
           Stdlib.Format.force_newline ())
      done>.
*)

let _ = Runcode.run f [|41;41;41;41;42;42;42;43;43;41|]
(*
41
4
42
3
43
2
41
1
*)

let _ = Runcode.run f [||]

let _ = Runcode.run f [|41;41;41;41;42;42;42;43;43|]
(*
41
4
42
3
43
2
*)

let _ = Runcode.run f [|41|]
(*
41
1
*)

(* Generate C code *)
module CC = Backends.C
module M = RLL(CC)
let _ =
  let open CC in
  pp_proc ~name:"rll" Format.std_formatter @@
  arg_base ~name:"n" tint @@ fun n ->
  arg_array ~name:"a" n tint @@ fun a ->
  nullary_proc (M.rll_print a)

(*
void rll(int64_t const n_1,int64_t * const a_2){
  int64_t x_3 = 0;
  int64_t x_4 = 0;
  bool x_5 = false;
  int64_t x_6 = 0;
  while (x_6 <= n_1)
  {
    int64_t const t_7 = x_6;
    x_6++;
    if (t_7 < n_1)
    {
      int64_t const t_9 = a_2[t_7];
      if (x_5)
      {
        int64_t const t_10 = x_4;
        x_4 = t_9;
        if (!(t_10 == t_9))
        {
          int64_t const t_11 = x_3;
          x_3 = 0;
          printf("%ld\n",t_10);
          printf("%ld\n",t_11 + 1);
        }
        else 
          x_3++;
      }
      else {
        x_4 = t_9;
        x_5 = true;
      }
    }
    else {if (x_5)
            {
              int64_t const t_8 = x_3;
              x_3 = 0;
              printf("%ld\n",x_4);
              printf("%ld\n",t_8 + 1);
            }}
  }
}
*)


let _ = print_endline "\nAll done"
