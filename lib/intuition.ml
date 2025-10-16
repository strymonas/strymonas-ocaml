(* Intuitive explanation of normalization used in the paper *)

(* 
#use "intuition.ml";;
*)

#directory "backends/Trx";;
#load "stream.cma";;

(* Function composition *)
let (>>) f g = fun x -> f x |> g

(* The identity function that makes CPS convenient *)
let (let-) c k = c k

(* Select the MetaOCaml backend, to generate OCaml code *)
(* Could have used the C backend as well *)
module C = Backends.MetaOCaml

(* Simplified Strymonas raw interface *)

type 'a stm = 'a C.stm
type 'a exp = 'a C.exp
type 'a mut = 'a C.mut

type 'a emit = ('a -> unit stm) -> unit stm

(* A stream is the generator, guard, and state introduction
   linear means the generator is linear
 *)
type 'a stream =
  | Flat : 'a flat -> 'a stream
  | Init : ('b exp * ('b mut -> 'a stream)) -> 'a stream
 and 'a flat = {unr: 'a emit;  grd:bool exp; linear:bool}

let initializing_ref  : 'z exp -> ('z mut -> 'a stream) -> 'a stream =
  fun i k -> Init (i,k)

let rec map_flat : type a b. (a flat -> b flat) -> (a stream -> b stream) =
  fun f -> function
    | Init (i,k) -> Init (i, k >> map_flat f)
    | Flat x -> Flat (f x)

(* Create an infinite stream: run step in an infinite loop
   The generator is assumed linear!
 *)
let infinite : 'a emit -> 'a stream = fun unr -> 
  Flat {unr; grd = C.bool true; linear=true}

(* An application of equational law (13): guard-guard *)
let guard : bool C.exp -> 'a stream -> 'a stream = fun g -> 
  map_flat (function {unr;grd;linear} -> {unr;linear;grd= C.(g && grd)})

(* Application of the equational laws (14) and (12); 
   guard-map_filter and guard-unroll
*)
(* The optional argument, ?linear, tells if the transformer is
   linear -- that is, if the continuation ('b -> unit cde) is invoked
   exactly once (except, perhaps, at the very end)
   By default it is true.
   If the continuation ends up not being invoked in some cases
   (that is, map_raw behaves like map_option), be sure specify
   ~linear:false!
   On no occasion should continuation be invoked multiple times!
   (because we expect goon to be evaluated once each time
    before calling the continuation)
*)
let map_raw : ?linear:bool -> ('a -> 'b emit) -> 'a stream -> 'b stream = 
  fun ?(linear=true) f -> 
    map_flat (function {unr;grd;linear=l} ->
      {grd; linear=l && linear; unr=fun k -> unr (fun x -> f x k)})

(* A particular, frequently occurring case of the above *)
let map_raw' : ('a -> 'b) -> 'a stream -> 'b stream =
   fun f -> map_raw (fun e k -> k (f e))

(* A filter is a non-linear case of map_raw *)
let filter_raw : ('a -> bool exp) -> 'a stream -> 'a stream = fun f ->
  map_raw ~linear:false C.(fun x k -> if1 (f x) (k x))

let rec zip_raw : type a b. a stream -> b stream -> (a * b) stream = 
   fun st1 st2 ->
  match (st1,st2) with 
  | (Init (i,k),st2)  -> Init (i, fun x -> zip_raw (k x) st2)
  | (st1,Init (i,k))  -> Init (i, fun x -> zip_raw st1 (k x))
  | (Flat {linear=true;unr;grd},st2) ->
  (* Lift guard by the eq. law (18) and then apply Proposition 3.9 *)
  map_raw (fun b k -> unr (fun a -> k (a,b))) st2 |> 
  guard grd
  | (st1,(Flat {linear=true} as st2)) ->
      zip_raw st2 st1 |> map_raw' (fun (x,y) -> (y,x))
  | (st1,st2) -> failwith "linearize one of st1 or st2"


(* Consumer: the inverse of [infinite] *)
let rec iter : type a. (a -> unit stm) -> a stream -> unit stm = fun consumer ->
  function
    | Init (i,k) -> C.newref i (k >> iter consumer)
    | Flat {unr;grd} -> C.while_ grd (unr consumer)


(* The running example, from the paper and from TryFirst *)

(*
let ex2 : int stm = 
  iota C.(int 1) 
  |> map C.(fun e -> e * e) 
  |> filter C.(fun e -> e mod (int 17) > int 7) 
  |> take C.(int 10) 
  |> fold C.(+) C.(int 0)
*)

(* This example, desugared *)
let u1 = C.(fun e -> e * e)
let u2 = C.(fun e -> e mod (int 17) > int 7)
let ex2raw = zip_raw 
     (initializing_ref C.(int 10) @@ fun i ->
        let ud = C.(fun k -> decr i @. k ()) in let g1 = C.(dref i > int 0) in 
        infinite ud |> guard g1)
     (initializing_ref C.(int 1) @@ fun z ->
        let un = C.(fun k -> letl (dref z) @@ fun v ->  incr z @. k v) in
        infinite un |> map_raw (fun e -> C.letl (u1 e)) |> filter_raw u2) 
  |> map_raw' (fun (_,x) -> x) |> fun st -> let open C in
  let- zs = newref (int 0) in 
  iter (fun a -> zs := dref zs + a) st @. ret (dref zs)

(* This is *exactly* the code generated for the first example in the paper
   (and also the first example in TryFirst)
*)

(*
val ex2raw : int stm =
  C.Stm .<
   let v_1 = Stdlib.ref 0 in
   (let v_2 = Stdlib.ref 10 in
    let v_3 = Stdlib.ref 1 in
    while (! v_2) > 0 do
      let t_4 = ! v_3 in
      Stdlib.incr v_3;
      (let t_5 = t_4 * t_4 in
       if (t_5 mod 17) > 7 then (Stdlib.decr v_2; v_1 := ((! v_1) + t_5)))
      done);
   ! v_1>. 
*)

(* Test it *)
let[@warning "-8"] 853 = Runcode.run (C.to_code ex2raw)

(* Example of linearizing a nested stream *)
(*
let ex_nested = from_to 1 5  |> flat_map (fun x -> from_to x (x+3))
*)

(* The naive implementation, for reference *)
let ex_nested = List.init 5 succ  
    |>  List.concat_map (fun x -> List.init 4 ((+) x))

(*
[1; 2; 3; 4; 2; 3; 4; 5; 3; 4; 5; 6; 4; 5; 6; 7; 5; 6; 7; 8]
*)

let[@warning "-8"] 90 = ex_nested |> List.fold_left (+) 0

(* The same stream as a state machine *)
let () =
  let un n z = fun k -> let v = !z in incr z; if (v <= n) then k v in
  let g1 n z = !z <= n in
  let zouter = ref 1 in
  let xr = ref 0 in
  let zinner = ref 0 in
  let exception Exit of int in
  let linear_unr st k = 
    let rec goto = function
      | 3 -> if g1 5 zouter = false then goto 0 else
             un 5 zouter (fun x -> xr := x; zinner := x; goto 7);
             goto 3;                     (* skipped *)
      | 7 ->  if g1 (!xr + 3) zinner =false then goto 3 else
              un (!xr+3) zinner (fun y -> k y; goto 5);  (* exit *)
              goto 7;                     (* skipped *)
      | 0 -> raise (Exit 0)
      | 5 -> raise (Exit 5)
    in goto st
  in
  let rec loop st =
    let st =
      try linear_unr st (fun y -> print_int y; print_endline ""); 0 with
        Exit x -> x in
    if st = 5 then loop 7
  in loop 3

(* Closure-conversion of the inner-stream, in CPS
   The continuation receives xr (the reference cell that will hold
   the argument), the inner stream body (whose components, unrolling
   and the guard) are the functions of xr, and the sequence of statements to
   initialize the inner stream state, again, as the function of xr.
   For simplicity, only int xr are considered.
 *)
let closure_convert : 
    (int exp -> 'b stream) -> 
    (int mut * 'b flat * unit C.stm -> 'w stream) -> 'w stream =
  fun stf k ->
   initializing_ref C.(int 0) @@ fun xr ->
     let rec loop acc = function
       | Init (i,sk) -> 
           initializing_ref i @@ fun z -> loop C.(acc @. (z := i)) (sk z) 
       | Flat fl -> k (xr,fl,acc)
    in loop C.unit (stf C.(dref xr))

(* specialize to int stream for simplicity *)
(* The state machine is emulated without gotos, as a while loop
*)

let flat_map_raw : (int exp -> 'b stream) -> int exp stream -> 'b stream =
   fun inner_fn st -> 
   initializing_ref C.(int 1) @@ fun q -> 
   closure_convert inner_fn @@ 
     fun (xr,{unr=unri; grd=grdi},init_state) -> (* all may depend on xr *)
   let linearize {unr;grd} =
     let open C in
     let unr' k = 
     (q := dref q + int 2) @.
     while_ (logand (dref q) (int 2) <> int 0) @@ begin
     (if1 (dref q = int 3)
        (if_ grd (* advancing outer *)
           (unr (fun x -> (xr := x) @. init_state @. (q := int 7)))
           (q := int 0))) @.
     (if1 (dref q = int 7)
        (if_ grdi        (* advancing inner *)
             (unri (fun y -> k y @. (q := int 5)))
             (q := int 3)))
   end
   in
   {unr=unr'; grd =C.(dref q <> int 0); linear=true}
   in
   map_flat linearize st

(* Consider optimizations
  -- do-while loop (and perhaps goto?)
 *)

(* The same code in strymonas, using Raw (desugared) API *)

let un n z = 
  C.(fun k -> let- v = letl (dref z) in (incr z) @. if1 (v <= n) (k v))
let g1 n z = C.(dref z <= n)
let ex_nested = 
  let from_to m n = initializing_ref m @@ fun z ->
    infinite C.(fun k -> 
      let- v = letl (dref z) in (incr z) @. if1 (v <= n) (k v)) |>
    guard C.(dref z <= n) in
  from_to C.(int 1) C.(int 5) |>
  flat_map_raw C.(fun x -> from_to x (x + int 3))

(* to see the state machine, complete stream with the accumulating consumer,
   like in ex2norm
*)

let ex_nested_print = 
  ex_nested |> iter C.print_int

(*
      val ex_nested_print : unit stm =
  C.Stm .<
   let v_32 = Stdlib.ref 0 in
   let v_33 = Stdlib.ref 1 in
   let v_34 = Stdlib.ref (! v_32) in
   let v_35 = Stdlib.ref 1 in
   while (! v_33) > 0 do
     v_33 := ((! v_33) + 2);
     while (Stdlib.Int.logand (! v_33) 2) > 0 do
       (if (! v_33) = 3
        then
          (if (! v_35) <= 5
           then
             let t_37 = ! v_35 in
             (Stdlib.incr v_35;
              if t_37 <= 5 then (v_32 := t_37; v_34 := (! v_32); v_33 := 7))
           else v_33 := 0);
        if (! v_33) = 7
        then
          (if (! v_34) <= ((! v_32) + 3)
           then
             let t_36 = ! v_34 in
             (Stdlib.incr v_34;
              if t_36 <= ((! v_32) + 3)
              then
                ((Stdlib.Format.print_int t_36;
                  Stdlib.Format.force_newline ());
                 v_33 := 5))
           else v_33 := 3))
       done
     done>.
*)   

let _ = Runcode.run (C.to_code ex_nested_print)
;;

let[@warning "-8"] 90 = ex_nested |> fun st -> C.(let- zs = newref (int 0) in 
     iter (fun a -> zs := dref zs + a) st @. ret (dref zs)) |>
     C.to_code |> Runcode.run

(* simplify, remove duplicated tests. Pull the guard forward *)
let ex_nested = 
  let from_to m n = initializing_ref m @@ fun z ->
    infinite C.(fun k -> let- v = letl (dref z) in (incr z) @. (k v)) |>
    guard C.(dref z <= n) in
  from_to C.(int 1) C.(int 5) |>
  flat_map_raw C.(fun x -> from_to x (x + int 3))

(* Linearization is only useful in the context of zip *)
let ex_nested_zip =
  zip_raw ex_nested ex_nested |> 
  map_raw' C.(fun (x,y) -> cond (x = y) (int 1) (int 0)) |>
  fun st -> C.(let- zs = newref (int 0) in 
  iter (fun a -> zs := dref zs + a) st @. ret (dref zs))

let[@warning "-8"] 20 = Runcode.run (C.to_code ex_nested_zip)
  
;;
  

let () = print_endline "\nAll Done"
;;

 
