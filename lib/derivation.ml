(*
Idea:
 find primitive streams (basis of the stream space, so to speak)
 such that:
  -- all desired stream pipelines can be expressed in terms of it
  -- their compositions can be efficiently compiled to low-level language,
  without intermediate data structure (even of the fixed size, let
 alone variable size)

The possibility of decomposition into such basis already shows fusion.
*)

(* What interface do we want, for producing and transforming
   streams. (cf. stream_cooked.mli) 
*)

(*
#use "derivation.ml";;
*)
let (>>) f g = fun x -> f x |> g

module type stream0 = sig
  type 'a stream
  
  (* Producers *)
  val of_arr  : 'a array -> 'a stream
  val iota    : int -> int stream
  val from_to : ?step:int -> int -> int -> int stream
  val unfold  : ('z -> ('a*'z) option) -> 'z -> 'a stream

  (* Transformers *)
  val map        : ('a -> 'b) -> 'a stream -> 'b stream
  val flat_map   : ('a -> 'b stream) -> 'a stream -> 'b stream
  val filter     : ('a -> bool) -> 'a stream -> 'a stream
  val take       : int -> 'a stream -> 'a stream
  val take_while : ('a -> bool) -> 'a stream -> 'a stream
  val drop       : int -> 'a stream -> 'a stream
  val drop_while : ('a -> bool) -> 'a stream -> 'a stream
  val map_accum  : ('z ->  'a -> 'b*'z) -> 'z -> 'a stream -> 'b stream
  val zip_with   : ('a -> 'b -> 'c) -> ('a stream -> 'b stream -> 'c stream)

   (* The observation function, possibly up to the given limit *)
  val observe : ?limit:int -> 'a stream -> 'a list
end


(* Denotational semantics *)

(* What is a stream: esstentially nu X. (1 + A * X) *)
type 'a skipstream = Nil | Cons of 'a * (unit -> 'a skipstream) 
       (* However, we need the following to give the semantics to filter,
          without resorting to bottoms -- making the filter productively
          coinductive
        *)
       | Skip of (unit -> 'a skipstream)

module Stream0Denot : (stream0 with type 'a stream = 'a skipstream) = struct
  type 'a stream = 'a skipstream

  (* Note: all the definitions below are productive *)

  let of_arr  : 'a array -> 'a stream = fun arr ->
    let rec gen i = 
      if i >= Array.length arr then Nil else Cons (arr.(i),fun () -> gen (i+1))
    in gen 0

  let rec iota : int -> int stream = fun i ->
    Cons (i, fun () -> iota (i+1))

  let rec from_to ?(step=1) : int -> int -> int stream = fun ifrom ito ->
    if step >= 0 then
      if ifrom > ito then Nil else 
         Cons (ifrom, fun () -> from_to ~step (ifrom+step) ito)
    else
      if ifrom < ito then Nil else 
         Cons (ifrom, fun () -> from_to ~step (ifrom+step) ito)

  let rec unfold  : ('z -> ('a*'z) option) -> 'z -> 'a stream = fun step z ->
    match step z with
    | None -> Nil
    | Some (a,z) -> Cons (a, fun () -> unfold step z)

  (* Transformers *)
  let (@@@) : ('a stream -> 'b stream) -> 
              (unit -> 'a stream) -> (unit -> 'b stream) = fun f th ->
    fun () -> th () |> f

  let rec map      : ('a -> 'b) -> 'a stream -> 'b stream = fun f -> function
    | Nil        -> Nil
    | Cons (x,t) -> Cons (f x, map f @@@ t)
    | Skip t     -> Skip (map f @@@ t)

  let rec filter   : ('a -> bool) -> 'a stream -> 'a stream =
    fun pred -> function
      | Nil -> Nil
      | Cons (x,t) -> 
          if pred x then Cons (x, filter pred @@@ t)
          else Skip (filter pred @@@ t)
      | Skip t     -> Skip (filter pred @@@ t)

  let rec take     : int -> 'a stream -> 'a stream = fun n -> function
    | Nil -> Nil
    | Cons (x,t) -> 
        if n <= 0 then Nil else 
        Cons (x, take (n-1) @@@ t)
    | Skip t     -> Skip (take n @@@ t)

  let rec take_while : ('a -> bool) -> 'a stream -> 'a stream =
    fun pred -> function
      | Nil -> Nil
      | Cons (x,t) -> 
          if pred x then Cons (x, take_while pred @@@ t)
          else Nil
      | Skip t     -> Skip (take_while pred @@@ t)
    
  let rec drop       : int -> 'a stream -> 'a stream = fun n -> function
    | Nil -> Nil
    | Cons (x,t) -> 
        if n > 0 then Skip (drop (n-1) @@@ t)
        else Cons (x, t)
    | Skip t     -> Skip (drop n @@@ t)

  let rec drop_while : ('a -> bool) -> 'a stream -> 'a stream =
    fun pred -> function
      | Nil -> Nil
      | Cons (x,t) -> 
          if pred x then Skip (drop_while pred @@@ t)
          else Cons (x, t)
      | Skip t     -> Skip (drop_while pred @@@ t)

  let rec flat_map : ('a -> 'b stream) -> 'a stream -> 'b stream = 
    fun f -> function
      | Nil        -> Nil
      | Skip t     -> Skip (flat_map f @@@ t)
      | Cons (x,t) -> 
          let rec inner stouter = function
            | Nil        -> Skip (flat_map f @@@ stouter)
            | Skip t     -> Skip (inner stouter @@@ t)
            | Cons (x,t) -> Cons (x, inner stouter @@@ t)
          in inner t (f x)
              
  let rec map_accum  : ('z->'a->'b*'z) -> 'z -> 'a stream -> 'b stream =
    fun f z -> function
      | Nil -> Nil
      | Cons (x,t) -> let (y,z) = f z x in Cons (y, map_accum f z @@@ t)
      | Skip t     -> Skip (map_accum f z @@@ t)

  let rec zip_with : ('a -> 'b -> 'c) -> ('a stream -> 'b stream -> 'c stream) =
    fun f s1 s2 -> match (s1,s2) with
    | (Nil,_) | (_,Nil) -> Nil
    | (Skip t, s2) -> Skip (fun () -> zip_with f (t ()) s2)
    | (s1, Skip t) -> Skip (fun () -> zip_with f s1 (t ()))
    | (Cons (x,t1), Cons (y,t2)) -> 
        Cons (f x y, fun () -> zip_with f (t1 ()) (t2 ()))

  let rec observe : ?limit:int -> 'a stream -> 'a list = fun ?limit -> function
    | Nil        -> []
    | _  when limit = Some 0 -> []
    | Skip t     -> t () |> observe ?limit
    | Cons (x,t) -> x :: (t () |> 
      observe ?limit:(match limit with None -> None | Some n -> Some (n-1)))
end

(* A few tests. Should be more than a few: ideally, all tests from
   ../test/ should appear here
 *)

module Test0(S:stream0) = struct[@warning "-8"]
    open S

  let [1;2;3] = 
    of_arr [|1.0;2.0;3.0|]
    |> map truncate
    |> observe

  let [4;16] = 
    of_arr [|1.0;2.0;3.0;4.0|]
    |> map truncate
    |> filter (fun x -> x mod 2 = 0)
    |> map (fun x -> x * x)
    |> observe

  let [1;2;5;10;17] =
    unfold (fun z -> if z < 5 then Some (z*z,z+1) else None) 0
    |> map (fun x -> x + 1)
    |> observe

  let [1;1;2;3;5;8] =
    unfold (fun (x,y) -> if x < 10 then Some (x,(y,x+y)) else None) (1,1)
    |> observe

  let [1;4;9;16;25;36;49;64;81;100] = 
    iota 1
    |> map (fun x -> x * x)
    |> take 10
    |> observe

  let [4;16;] = 
    of_arr [|1.0;2.0;3.0;4.0;5.0;6.0;7.0|]
    |> map truncate
    |> filter (fun x -> x mod 2 = 0)
    |> take 2
    |> map (fun x -> x * x)
    |> observe

  let [1; 2; 3; 4; 5] =
    from_to 1 5
    |> observe

  let [0; 1; 2; 1; 2; 3; 2; 3; 4; 3; 4; 5; 4; 5; 6; 5; 6; 7] =
    of_arr [|0;1;2;3;4;5|]
    |> flat_map (fun x -> of_arr [|x; x+1; x+2|])
    |> observe

  let even x = (x mod 2 = 0)
  let plus_arr arr = fun x -> of_arr arr |> map (fun c -> x + c)

  let [0; 1; 2; 2; 3; 4; 4; 5; 6] =
    of_arr [|0;1;2;3;4;5|]
    |> filter even
    |> flat_map (plus_arr [|0;1;2|])
    |> observe

  let [4; 6; 8; 10; 12; 14; 16; 18; 20; 22] =
  iota 1
  |> flat_map (fun x -> iota (x+1)) (* Infinite inner stream *)
  |> flat_map (fun x -> iota (x+1)) (* Infinite inner stream *)
  |> flat_map (fun x -> iota (x+1)) (* Infinite inner stream *)
  |> filter even
  |> take 10
  |> observe

  let [6; 8; 10; 12; 14; 16; 18; 20; 22; 24] =
  iota 1
  |> flat_map (fun x -> iota (x+1)) (* Infinite inner stream *)
  |> flat_map (fun x -> iota (x+1)) (* Infinite inner stream *)
  |> filter even
  |> flat_map (fun x -> iota (x+1)) (* Infinite inner stream *)
  |> filter even
  |> take 10
  |> observe

  let [2; 4; 4; 4; 6; 6; 6; 8; 8; 8] =
  iota 1
  |> flat_map (fun x -> iota (x+1) |> take 3)
  |> filter even
  |> take 10
  |> observe

  let [2; 3] =
    iota 1
    |> flat_map (fun x -> iota (x * 1)
                          |> flat_map (fun x -> iota (x * 2))
                          |> take 3 )
    |> take 2
    |> observe

  let [114; 114; 216; 114; 216; 114; 216; 216; 216; 120] =
  iota 1
  |> flat_map (fun x -> iota (x+10)
			|> flat_map (plus_arr [|100;200|])
			|> filter (fun x -> (x mod 3 = 0))
			|> take 3)
  |> filter even
  |> take 10
  |> observe

  let [(4, 100.); (16, 121.)] =
  zip_with (fun x y -> (x,y))
	    (
	      of_arr [|1.0;2.0;3.0;4.0;5.0;6.0|]
	      |> map truncate
	      |> take 5
	      |> filter even
	      |> map (fun x -> x * x))
	    (
	      iota 10
	      |> map (fun x -> float_of_int (x * x)))
  |> observe

  let [(4, 100.); (16, 121.)] =
  zip_with (fun x y -> (y,x))
	    (
	      iota 10
	      |> map (fun x -> float_of_int (x * x)))
	    (
	      of_arr [|1.0;2.0;3.0;4.0;5.0;6.0|]
	      |> map truncate
	      |> take 5
	      |> filter even
	      |> map (fun x -> x * x))  
  |> observe

  let [(1., 2); (2., 3); (3., 4); (4., 3); (5., 4)] =
    zip_with (fun x y -> (x,y))
	    (
	      of_arr [|1.0;2.0;3.0;4.0;5.0;6.0|]
	      |> take 5)
	    (
	      iota 1
	      |> flat_map (fun x -> iota (x+1) |> take 3))
  |> observe

  let [(0, 2); (2, 3); (2, 2); (2, 3); (2, 4); (4, 5)] =
    zip_with (fun x y -> (x,y))
      (of_arr [|0;1;2;3|]
       |> flat_map (plus_arr [|0;1|])
       |> flat_map (plus_arr [|0;1|])
       |> filter even)
      (of_arr [|1;2;3|]
       |> flat_map (plus_arr [|0;1|])
       |> filter even
       |> flat_map (plus_arr [|0;1|]))
  |> observe

  let [(2, 2); (5, 5); (8, 8); (11, 11)] =
    zip_with (fun x y -> (x,y))
      (from_to 0 56 |> filter (fun e -> (e mod 3) = 2))
      (from_to 1 13 |> filter (fun e -> (e mod 3) = 2))
    |> observe

  let
    [(1., (1., 2)); (2., (2., 3)); (3., (3., 4)); (4., (4., 3)); (5., (5., 4))]
    = 
    zip_with (fun x y -> (x,y))
      (of_arr [|1.0;2.0;3.0;4.0;5.0;6.0|] |> take 5)
      (zip_with (fun x y -> (x,y))
	(of_arr [|1.0;2.0;3.0;4.0;5.0;6.0|] |> take 12)
	(iota 1 |> flat_map (fun x -> iota (x+1) |> take 3)))
    |> observe

  (* "zip: testzff1" *)
  let [(10, 110); (11, 111); (12, 112); (13, 120); (14, 121); 
       (20, 122); (21, 130); (22, 131); (23, 132); (24, 140); 
       (30, 141); (31, 142)] =
    zip_with (fun x y -> (x,y))
      (of_arr [|10;20;30|] |>
         flat_map (fun e -> iota e |> take 5))
      (of_arr [|10;20;30;40|] |>
         flat_map (fun e -> iota (100 + e) |> take 3))
    |> observe

  let [1;2;3;4] =
    iota 1
    |> take_while (fun x -> x mod 5 <> 0)
    |> observe

  let [] =
    iota 1
    |> take_while (fun x -> false)
    |> observe

  let [1;2;3;4] =
    iota 1
    |> drop 0
    |> take_while (fun x -> x mod 5 <> 0)
    |> observe

  let [6;7;8;9] =
    iota 1
    |> drop 5
    |> take_while (fun x -> x mod 5 <> 0)
    |> observe

  let [] =
    iota 1
    |> drop 5
    |> take_while (fun x -> x mod 5 <> 0)
    |> drop 5
    |> observe

  let [5;6;7;8;9;10;11] =
    iota 1
    |> drop_while (fun x -> x mod 5 <> 0)
    |> take 7
    |> observe

  let  (* "test_drop6" *)
      [(8,18); (10,21); (12,24); (14,27); (16,30)] =
    zip_with (fun x y -> (x,y))
      (iota 1 |> filter (fun x -> x mod 2 = 0) |> drop 3)
      (iota 1 |> filter (fun x -> x mod 3 = 0) |> drop 5)
    |> take 5
    |> observe

  let (* "test_drop_while2" *)
      [(3,5); (4,6); (5,7); (6,8); (7,9)] =
    zip_with (fun x y -> (x,y))
      (iota 1 |> drop_while (fun e -> e < 3))
      (iota 1 |> drop_while (fun e -> e < 5))
    |> take 5 
    |> observe

  let (* "test_take_while2" *)
      [(1,1); (2,2);] =
    zip_with (fun x y -> (x,y))
      (iota 1 |> take_while (fun e -> e < 3))
      (iota 1 |> take_while (fun e -> e < 5))
    |> take 5 
    |> observe

  let [(1, 1); (1, 2); (2, 4); (4, 7); (7, 11)] =
    iota 0
    |> map_accum (fun z x -> ((z,z+x),z+x)) 1
    |> take 5
    |> observe

  (* example of normalization, see the end of the file *)
  let [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] =
    iota 1 |> flat_map (fun x -> from_to x (x+5)) |> take 10
    |> observe
end

module M0 = Test0(Stream0Denot)

(* Main idea: from the model, abduce the algebra *)


(* The interface can be easily reduced to more core constructs *)
module type stream1 = sig
  type 'a stream
  
  (* Producers *)
  val pull_array : int -> (int -> 'a) -> 'a stream
  val from_step  : int -> int -> int stream
  val unfold     : ('z -> ('a*'z) option) -> 'z -> 'a stream

  (* Transformers *)
  val map_accum  : ('z ->  'a -> 'b*'z) -> 'z -> 'a stream -> 'b stream
  val flat_map   : ('a -> 'b stream) -> 'a stream -> 'b stream
  val filter     : ('a -> bool) -> 'a stream -> 'a stream
  val take_while : ('a -> bool) -> 'a stream -> 'a stream
  val zip        : 'a stream -> 'b stream -> ('a*'b) stream

   (* The observation function, possibly up to the given limit *)
  val observe : ?limit:int -> 'a stream -> 'a list
end

(* We could also express filter in terms of flat_map (as we did in
   strymonas v1)
   filter pred === 
     flat_map (fun x -> if pred x then singleton x else empty)

but that tends to produce poor code.
   Filter is much smpler than flat_map and has many more useful laws
*)

(* Reduce stream0 to a more core stream1 *)
module Desugar10(S:stream1) : stream0 = struct
  type 'a stream = 'a S.stream
  
  (* Producers *)
  let of_arr  : 'a array -> 'a stream = fun arr ->
    S.pull_array (Array.length arr - 1) (fun i -> arr.(i))

  let iota : int -> int stream = fun n -> S.from_step n 1

  let from_to ?(step=1) : int -> int -> int stream = fun ifrom ito ->
    if step = 1 then S.pull_array (ito - ifrom) (fun i -> i + ifrom)
    else
    S.from_step ifrom step |>
    S.take_while (if step > 0 then (fun x -> x <= ito) else (fun x -> x >= ito))
    
  let unfold  : ('z -> ('a*'z) option) -> 'z -> 'a stream = S.unfold

  let map        : ('a -> 'b) -> 'a stream -> 'b stream = fun f st ->
    S.map_accum (fun z x -> (f x,z)) () st

  let map_accum  : ('z ->  'a -> 'b*'z) -> 'z -> 'a stream -> 'b stream =
    S.map_accum

  let flat_map   : ('a -> 'b stream) -> 'a stream -> 'b stream = 
    S.flat_map

  let filter     : ('a -> bool) -> 'a stream -> 'a stream = S.filter

  let take       : int -> 'a stream -> 'a stream = fun n st ->
    S.zip st (from_to 1 n) |> map (fun (x,y) -> x)

  let take_while : ('a -> bool) -> 'a stream -> 'a stream = S.take_while

  (* drop can be implemented in terms of other primitives, albeit
     not very efficiently: we don't need to keep incrementing counter
     once we finished dropping
   *)
  let drop       : int -> 'a stream -> 'a stream = fun n st ->
    st |> S.zip (iota 0) |> S.filter (fun (i,_) -> i < n) |> map snd

  (* Here is a better implementation *)
  let drop       : int -> 'a stream -> 'a stream = fun n st ->
    st |> S.map_accum (fun z x -> 
      let z = if z <= n then z+1 else z in ((x,z),z)) 0 |>
    S.filter (fun (_,z) -> z > n) |> map fst

  (* For drop_while, we really need accumulation *)
  let drop_while : ('a -> bool) -> 'a stream -> 'a stream = fun pred st ->
    st |> S.map_accum (fun z x -> let z = z && pred x in ((x,z),z)) true |>
    S.filter (fun (_,p) -> not p) |> map fst

  let zip_with : ('a -> 'b -> 'c) -> ('a stream -> 'b stream -> 'c stream) =
    fun f s1 s2 -> S.(zip s1 s2 |> map (fun (x,y) -> f x y))

  let observe = S.observe
end

module Stream1Denot : (stream1 with type 'a stream = 'a skipstream) = struct
  include Stream0Denot

  (* Note: all the definitions below are productive *)

  let pull_array : int -> (int -> 'a) -> 'a stream = fun upb f ->
    let rec loop i = 
      if i <= upb then Cons (f i, fun () -> loop (i+1)) else Nil
    in loop 0        

  let rec from_step : int -> int -> int stream = fun ifrom step ->
    Cons (ifrom, fun () -> from_step (ifrom+step) step)

  let rec zip : 'a stream -> 'b stream -> ('a*'b) stream =
    fun s1 s2 -> match (s1,s2) with
    | (Nil,_) | (_,Nil) -> Nil
    | (Skip t, s2) -> Skip (fun () -> zip (t ()) s2)
    | (s1, Skip t) -> Skip (fun () -> zip s1 (t ()))
    | (Cons (x,t1), Cons (y,t2)) -> 
        Cons ((x,y), fun () -> zip (t1 ()) (t2 ()))

  let (@@@) : ('a stream -> 'b stream) -> 
              (unit -> 'a stream) -> (unit -> 'b stream) = fun f th ->
    fun () -> th () |> f
end

module M1 = Test0(Desugar10(Stream1Denot))

(* Now, let's start derivation *)

(* First observation: pull_array and unfold fuse with map, obviously
   (actually, with map_accum: see later on, for concreteness)
   But from_step obviously does not. Yet it is useful: a primitive
   infinite stream. OTH, unfold seems composite: the termination
   condition is well expressed by take_while.

   There is a subtle difference: with unfold, we can express that we tried
   to get an element, given the state, and in the process discovered that
   the stream is finished: think of EOF when reading a file. 
   Unfold also works well for the continuation
   semantics: when finished, just jump to the exit continuation.
   OTH, it doesn't work well with structural programming: while loop.
   Here, we would like to have an exit test first. But what if the exit
   test succeeded, we tried to get an element and then discovered that
   the stream is finished? We need a way to skip -- don't produce an element
   but continue with the stream. We need such facility anyway to implement
   filter or windowing. So, if we tried to get an element from the state and
   found the stream is finished, we skip. The next iteration will check the
   exit condition, finds it is set, and terminates the stream.
   Granted, we waste one iteration of the while loop. It is the last one,
   so the price is tolerable.
*)
(* 
 Second observation: look at drop and drop_while. Clearly they are dealing with
 a stateful stream: which carries not just elements but also a state.
 Primitives may examine the state.
 Let's thus make the state explicit.

*)

module type stream2 = sig
  type ('a,'z) stream
  
  (* Producers *)
  (* produces an infinite stream, always
     (although it may be an infinite skipping stream: after some
     time it keeps skipping)
  *)
  val unroll     : ('z -> 'a option * 'z) -> 'z -> ('a,'z) stream
  (* produces a finite stream. We make it also stateful, to match
    unroll
   *)
  val pull_array : 'z -> int -> ('z -> int -> ('a*'z)) -> ('a,'z) stream

  (* Introducing a new piece of state. This combinator is not strictly needed:
     all state needed for the pipeline can be introduced by the producer.
     But it is not modular, and cumbersome -- not just in practice but
     also for formalization.
   *)
  val init : 'z -> ('a,'z1) stream -> ('a,'z*'z1) stream

  (* Transformers *)
  (* Stateful map_filter_accumulator.
     Although it may, in principle, be updating the producer's state,
     it should generally use its own, introduced by init.
   *)
  val map_filter  : ('z -> 'a -> 'b option*'z) -> ('a,'z) stream -> 
   ('b,'z) stream

  (* Can only examine the state of the producer, not the elements 
     That is crucial for the commutation laws later on.
     Also, it does not update the state.
     The are further side-conditions detailed below, in equational laws.
   *)
  val guard : ('z -> bool) -> ('a,'z) stream -> ('a,'z) stream

  val zip : ('a,'z1) stream -> ('b,'z2) stream -> ('a*'b,'z1*'z2) stream

  (* The inner stream may of course have its own state -- which
     should be hidden/abstracted.
     What is exposed is the state of the outer stream, which the inner
     may access.
  *)
  val flat_map   : ('z -> 'a -> ('b,'z) stream) ->
                   ('a,'z) stream -> ('b,'z) stream

  (* Here, 'z1 -> 'z2 must be a bijection!
     It is needed to reshuffle state, say, from (z1*z2)*z3 to z1*(z2*z3)
  *)
  val adjust : (('z1 -> 'z2) * ('z2 -> 'z1)) -> 
               ('a,'z1) stream -> ('a,'z2) stream

  (* Hide a part of the state *)
  val abstract : ('a, 'zp*'z) stream -> ('a,'z) stream

   (* The observation function, possibly up to the given limit *)
   (* State is _not_ observed: it is internal and may even be abstracted away
    *)
  val observe : ?limit:int -> ('a,'z) stream -> 'a list
end

(* XXX Need tests for stream2 specifically *)

module Desugar21(S:stream2) : stream1 = struct
  type 'a stream = St : ('a,'z) S.stream -> 'a stream (* hide state *)

  let to_stream : ('a,'z) S.stream -> 'a stream = fun st -> St st

  let pull_array : int -> (int -> 'a) -> 'a stream = fun n step ->
    S.pull_array () n (fun z i -> (step i,z)) |> to_stream

  let from_step  : int -> int -> int stream = fun from step ->
    S.unroll (fun z -> (Some z, z+step)) from |> to_stream

  let observe : ?limit:int -> 'a stream -> 'a list = fun ?limit (St st) ->
    S.observe ?limit st

  let unfold     : ('z -> ('a*'z) option) -> 'z -> 'a stream = fun step z ->
    let ustep z = match z with
    | None   -> (None,None)
    | Some z -> match step z with
    | Some (x,z) -> (Some x,Some z)
    | None       -> (None,None)
    in
    S.unroll ustep (Some z) |>  
    S.guard ((<>) None) |>
    to_stream

  let map_accum  : ('z ->  'a -> 'b*'z) -> 'z -> 'a stream -> 'b stream =
    fun f z (St st) ->
      S.init z st |>
      S.map_filter (fun (z,z1) a -> let (b,z') = f z a in (Some b,(z',z1))) |> 
      to_stream

  let filter     : ('a -> bool) -> 'a stream -> 'a stream = fun pred (St st) ->
    S.map_filter (fun z a -> ((if pred a then Some a else None), z)) st |> 
    to_stream

  (* It looks simple: just make the pred a guard. But guard looks only at the
     state, not at the element. Then push the element into state.
     But the guard is evaluated on the initial state, before the element
     is produced!
  *)
  let take_while : ('a -> bool) -> 'a stream -> 'a stream = fun pred (St st) ->
    S.init true st |>            (* introduce new state *)
    S.map_filter (fun (z,z1) a -> (* Effectively, may end with a Skip state *)
      if pred a then (Some a,(z,z1)) else (None,(false,z1))) |> 
    S.guard (fun (z,z1) -> z) |> 
    to_stream

  let zip        : 'a stream -> 'b stream -> ('a*'b) stream = 
    fun (St st1) (St st2) -> S.zip st1 st2 |> to_stream

  let swap (x,y) = (y,x)

  let flat_map : ('a -> 'b stream) -> 'a stream -> 'b stream = 
    fun f (St sto) ->
      let f' z a = match f a with
      | St sti -> S.init z sti |> S.adjust (swap,swap) |> S.abstract 
      in
      S.flat_map f' sto |> to_stream
end

(* What is a stream: esstentially nu X. (1 + A * X) 
   State is threaded all throughout

   The final state SNil does not carry state however: first, part of
   it is private anyway. Mainly, in case of zip st1 st2, when st2 has
   ended but st1 has not, the final state is not well-defined: it depends
   if zip first tries to advance st1 or st2 -- which is unspecified.

   Alternative (perhaps better for formalization?)
   | SCons : 'a * ('zp*'z) * (('zp * 'z) -> ('a,'z) ststream) -> 
     ('a,'z) ststream
   where 'zp is existential. That is, some part of the state is hidden.
*)
type ('a,'z) ststream = 
  | SNil
  | SCons of ('a option * 'z) * ('z -> ('a,'z) ststream) 


(* Watch that the state of the stream 'z is used strictly 
   linearly and NOT captured in closures!
   A rule-of-thumb: avoid explicit lambdas.
 *)
module Stream2Denot : (stream2 with type ('a,'z) stream = ('a,'z) ststream) = 
 struct
  type ('a,'z) stream = ('a,'z) ststream

  (* Perhaps make the first element to be SCons (None,z0) ...,
     to preserve the initial state?
  *)
  let rec unroll     : ('z -> 'a option * 'z) -> 'z -> ('a,'z) stream =
    fun ustep z -> SCons (ustep z, unroll ustep)

  let pull_array : 'z -> int -> ('z ->int ->('a*'z)) -> ('a,'z) stream =
    fun z n step ->
      let rec loop i z = 
        if i > n then SNil else
        let (a,z) = step z i in SCons ((Some a, z), loop (i+1))
      in loop 0 z

  (* functional composition *)
  let (@@@) : (('a,'z) stream -> ('b,'z1) stream) -> 
              ('z -> ('a,'z) stream) -> ('z1 -> ('b,'z1) stream) = 
    fun f th -> fun z -> th z |> f

  let pair_fold : ('a1 -> 'b1 -> 'c) -> ('a -> 'a1) -> ('b -> 'b1) -> 
    ('a * 'b) -> 'c = fun f f1 f2 -> fun (a,b) ->
      f (f1 a) (f2 b)

  let rec init : 'z -> ('a,'z1) stream -> ('a,'z*'z1) stream = 
    fun z -> function
      | SNil   -> SNil
      | SCons ((ao,z1),t) -> SCons ((ao,(z,z1)), pair_fold (@@) init t)

  let rec map_filter  : ('z -> 'a -> 'b option*'z) -> ('a,'z) stream -> 
   ('b,'z) stream = 
    fun f -> function
      | SNil   -> SNil
      | SCons ((Some a,z),t) -> SCons (f z a,    map_filter f @@@ t)
      | SCons ((None,z),t)   -> SCons ((None,z), map_filter f @@@ t) 

  let rec guard : ('z -> bool) -> ('a,'z) stream -> ('a,'z) stream =
    fun pred -> function
      | SCons ((ao,z),t) when pred z -> SCons ((ao,z), guard pred @@@ t)
      | _ -> SNil

  let rec flat_map   : ('z -> 'a -> ('b,'z) stream) ->
                       ('a,'z) stream -> ('b,'z) stream =
    fun f -> function
      | SNil    -> SNil
      | SCons ((None,z),t)  -> SCons ((None,z), flat_map f @@@ t)
      | SCons ((Some a,z),t) -> 
          let rec inner z stouter = function
            | SNil         -> SCons ((None,z),  flat_map f @@@ stouter)
            | SCons (az,t) -> SCons (az, fun z -> inner z stouter @@ t z)
          in inner z t (f z a)

  let rec zip : ('a,'z1) stream -> ('b,'z2) stream -> ('a*'b,'z1*'z2) stream =
    fun s1 s2 -> match (s1,s2) with
    | (SNil,_) | (_,SNil) -> SNil
    | (SCons ((None,z1),t1), SCons ((None,z2),t2)) -> 
        SCons ((None,(z1,z2)), pair_fold zip t1 t2)
    (* Here we don't advance the state of the first stream,
       even though we have peeked at its element.
       One can say, after looking at s1, we reset it.
       As we are talking about denotational semantics, this is what we want
     *)
    | (SCons ((_,z1),_), SCons ((None,z2),t2)) -> 
        SCons ((None,(z1,z2)), pair_fold zip (Fun.const s1) t2)
    | (SCons ((None,z1),t1), SCons ((_,z2),_)) -> 
        SCons ((None,(z1,z2)), pair_fold zip t1 (Fun.const s2))
    | (SCons ((Some x,z1),t1), SCons ((Some y,z2),t2)) -> 
        SCons ((Some (x,y),(z1,z2)), pair_fold zip t1 t2)

  let rec adjust : (('z1 -> 'z2) * ('z2 -> 'z1)) -> 
                   ('a,'z1) stream -> ('a,'z2) stream = 
    fun (f,g) -> function
      | SNil           -> SNil
      | SCons ((ao,z1),t) -> 
          SCons ((ao, f z1), fun z2 -> adjust (f,g) @@ t @@ (g z2))

  (* Here we create a closure deliberately, to abstract out a part
     of state. We could have used some sort of existenttial, but
     it is inconvenient.
  *)
  let rec abstract : ('a, 'zp*'z) stream -> ('a,'z) stream = function
    | SNil -> SNil
    | SCons ((ao, (zp,z)), t) -> SCons ((ao, z), fun z -> t (zp,z) |> abstract)

  let rec observe : ?limit:int -> ('a,'z) stream -> 'a list = 
    fun ?limit -> function
      | SNil     -> []
      | _  when limit = Some 0 -> []
      | SCons ((None,z),t)   -> t z |> observe ?limit
      | SCons ((Some x,z),t) -> x :: (t z |> 
        observe ?limit:(match limit with None -> None | Some n -> Some (n-1)))
end

module M2 = Test0(Desugar10(Desugar21(Stream2Denot)))

(* Equations among streams, which give us algebra *)
module Equations2(S:stream2) = struct
  open S

  let (===) : ('a,'z) stream -> ('a,'z) stream -> 
              ('a,'z) stream * ('a,'z) stream =
    fun x x -> (x,x)

  (* init can be fused with a producer *)
  let init_pull ((z,upb,step),z1) =
    pull_array z upb step |> init z1 ===
    let z' = (z1,z) in
    let step' (z1,z) i = 
      let (a,zn) = step z i in (a, (z1,zn))
    in
    pull_array z' upb step'

  let init_unroll ((z,step),z1) =
    unroll step z |> init z1 ===
    let z' = (z1,z) in
    let step' (z1,z) = 
      let (a,zn) = step z in (a, (z1,zn))
    in
    unroll step' z'

  (* init can move in past anything, and abstract can move out past everything
   *)

  let init_map (st,f,z) =
    st |> map_filter f |> init z ===
    let f' (z,z1) a =
      let (b,z1') = f z1 a in (b, (z,z1'))
    in
    st |> init z |> map_filter f'

  let abstract_map (st,f) =
    st |> abstract |> map_filter f ===
    let f' (z,z1) a =
      let (b,z1') = f z1 a in (b, (z,z1'))
    in
    st |> map_filter f' |> abstract

  let init_guard (st,pred,z) =
    st |> guard pred |> init z ===
    let pred' (z,z1) = pred z1
    in
    st |> init z |> guard pred'

  let abstract_guard (st,pred) =
    st |> abstract |> guard pred ===
    let pred' (z,z1) = pred z1
    in
    st |> guard pred' |> abstract


  let adjust_z_z1z2 = fun st -> st |> 
     adjust ((fun (z,(z1,z2)) -> ((z,z1),z2)),
            (fun ((z,z1),z2) -> (z,(z1,z2))))

  let init_zip (st1,st2,z) =
    zip (init z st1) st2 ===
    (zip st1 st2 |> init z |> adjust_z_z1z2)

  let adjust_z1z2_z = fun st -> st |> 
     adjust ((fun ((z,z1),z2) -> (z,(z1,z2))),
             (fun (z,(z1,z2)) -> ((z,z1),z2)))

  let abstract_zip (st1,st2) =
    zip (st1 |> abstract) st2 ===
    (zip st1 st2 |> adjust_z1z2_z |> abstract)

 (* For flat_map, init cannot
    move out the inner stream, obviously (unless the init expression
    is the constant).
  *)
  let init_flatmap (st,f,z) =
    st |> flat_map f |> init z ===
    let f' (z,z1) a =
      let sti = f z1 a in sti |> init z
    in
    st |> init z |> flat_map f'

  let abstract_flatmap (st,f) =
    st |> abstract |> flat_map f ===
    let f' (z,z1) a =
      let sti = f z1 a in sti |> init z
    in
    st |> flat_map f' |> abstract

  let init_abstract (st,z) =
    st |> abstract |> init z ===
    (st |> init z |> adjust ((fun (z,(z1,z2)) -> (z1,(z,z2))),
                             (fun (z1,(z,z2)) -> (z,(z1,z2)))) |> abstract)

  let abstract_init (st,z) =
    st |> init z |> abstract  === st


 (* Pull array can be converted to unroll: we need an extra state though.
  *)
  let pullarray_unroll (upb,z,step) =
    pull_array z upb step ===
    let z2 = (0,z) in
    let step2 (i,z) =
      if i <= upb then
        let (a,z) = step z i in 
        (Some a, ((i+1),z))
      else (None, (i,z))
    in
    unroll step2 z2 |> guard (fun (i,_) -> i <= upb) |> abstract

  (* pull_array cannot be fused with map_filter since it does not allow
     for skipping. Although we can fuse if map_filter does no skipping
     (i.e., filtering): a proper map
  *)

  let map_unroll ((step,z),f) =
    unroll step z |> map_filter f ===
    let step' z = match step z with
    | (None, z) -> (None, z)
    | (Some a,z) -> f z a
    in
    unroll step' z

  let guard_guard (pred1,pred2,st) = 
    st |> guard pred1 |> guard pred2 ===
    (st |> guard (fun x -> pred1 x && pred2 x))

  (* Side-conditions!!
     The following law looks very simple: because in OCaml we can only
     check that types are the same. map_filter and guard can indeed commute
     without affecting the types.
     Semantically however, map and filter can commute only upon the following
     side-conditions:
     (1) mapping function does not change the part of the state used by
     guard:
        let (b,z') = f z a in pred z' === pred z
        for all a and z.
    That is, if map needs state, it allocates the private state for itself
    using init. This is how it happens in strymonas all the time.
    (2) OR, more generally:
        pred z = false   ==>  let (b,z') = f z a in pred z' = false
        AND
        pred z = true && let (b,z') = f z a in pred z' = false  ==>
        b = None
     That is, if the guard fails it stays false no matter how state further
     evolves. If the mapping affects the state in such a way that causes
     the further guard to fail, it should skip.
     (example: see take_while in stream_cooked_fn.ml).
     In that case, the stream equivalence holds only weakly: the streams
     are distinguished by the final Skip state.
     In strymonas, we typically ensure the the first part of (2) by
     allocating a fresh mutable cell for the guard result, initialized
     to true. Whenever we assigned anything to the cell we verify
     if it is not already false. That is, we use only a monotone mutation.
  *)

  let map_guard (f,pred,st) =
    st |> guard pred |> map_filter f ===
    (st |> map_filter f |> guard pred)

  (* Side-conditions!!
     The inner guard may be pulled out 
     The same side-conditions as map_guard
  *)
  let guard_flatmap1 (pred,f,st) =
    st |> guard pred |> flat_map f ===
    (st |> flat_map f |> guard pred)


  let map_flatmap (f1,f2,st) =
    st |> flat_map f1 |> map_filter f2 ===
    let f z x = 
      let sti = f1 z x in
      map_filter f2 sti
    in 
    st |> flat_map f 

  let flatmap_flatmap (f1,f2,st) =
      st |> flat_map f1 |> flat_map f2 ===
      let f z a =
        let st1 = f1 z a in  
        flat_map f2 st1
      in
      st |> flat_map f

  let zip_pullarray_pullarray ((upb1,z1,step1), (upb2,z2,step2)) =
    zip (pull_array z1 upb1 step1) (pull_array z2 upb2 step2) ===
    let upb = min upb1 upb2 in
    let z   = (z1,z2) in
    let step (z1,z2) i = 
      let (a,z1) = step1 z1 i in
      let (b,z2) = step2 z2 i in
      ((a,b),(z1,z2))
    in
    pull_array z upb step

  (* Side-conditions!!
     The following holds only in the case when stream2 is linear:
     Once it starts skipping, it never returns an element any more
     (essentially finished):
     forall z. fst (step2 z) = None ===> fst (step2 (snd (step2 z))) = None
     Usually then there is a guard that terminates the stream once it
     starts skipping.
     Here, the stream equivalence holds only weakly!
   *)
  let zip_unroll_unroll ((z1,step1), (z2,step2)) =
    zip (unroll step1 z1) (unroll step2 z2) ===
    let z = (z1,z2) in
    let step (z1,z2) =
      match step1 z1 with
      | (None,z1) -> (None, (z1,z2))
      | (Some x,z1) -> match step2 z2 with
        | (Some y,z2) -> (Some (x,y), (z1,z2))
        | (None, z2)  -> (None, (z1,z2))    (* Throwing out x! *)
    in
    unroll step z

   (* In general, zip of two unroll streams cannot be written as unroll,
      without adding anything to the state. The case `throwing out x'
      above is a show-stopper. What we can do is to enhance the state:
   *)
  let zip_unroll_unroll_ext ((z1,step1), (z2,step2)) =
    zip (unroll step1 z1) (unroll step2 z2) ===
    let z = ((None,z1),z2) in
    let step (z1',z2) =
      match z1' with
      | (Some x,z1) -> begin
          match step2 z2 with
          | (None, z2) -> (None, ((Some x,z1),z2))
          | (Some y, z2) -> (Some (x,y), ((None,z1), z2))
          end      
      | (None,z1) -> match step1 z1 with
      | (None,z1) -> (None, ((None,z1),z2))
      | (Some x,z1) -> match step2 z2 with
        | (Some y,z2) -> (Some (x,y), ((None,z1),z2))
        | (None, z2)  -> (None, ((Some x,z1),z2))    
    in
    unroll step z |> 
    adjust ((fun ((z,z1),z2) -> (z,(z1,z2))),
            (fun (z,(z1,z2)) -> ((z,z1),z2))) |> abstract

    (* This is like `putting back' the value of stream1 for later use, 
       when stream2 stuttered.
   The drawback is that we have to introduce 'a option into the state, and
   build and deconstruct Some _ values (which is cumbersome).
   Generally, it produces suboptimal code
   *)

   (*
   The second option, which leads to a better code, is converting a general
   unroll stream to a linear stream.
   Actually, we can't convert the general unroll, but only unroll coupled
   with a guard.
   We may only stutter (fail to produce an element) when we are sure
   the stream will be stopped by some guard.
   *)
  let unroll_linear ((step,z),pred) =
    unroll step z |> guard pred ===
    let rec step' z = 
      match step z with
      | (None,z) when pred z -> step' z (* `continue' *)
      | e -> e 
    in
    unroll step' z |> guard pred

   (* In fact, if unroll z1 step1 is a linear stream, then zipping may
      be replaced with map_filter
   *)
  (* Side-conditions!!
     First case: stream1 never stutters
  *)
  let zip_linear1 ((z1,step1), st2) =
    zip (unroll step1 z1) st2 ===
    let f (z1,z2) y =
      match step1 z1 with
      | (Some x, z1) -> (Some (x,y),(z1,z2))
      | (None, z1)   -> assert false
    in
    init z1 st2 |> map_filter f

  (* Side-conditions!!
     Second case: stream1 may stutter, but only at the very end 
     forall z. fst (step1 z) = None ===> fst (step1 (snd (step1 z))) = None
     Equivalence here is only weak: map_filter actually advances st2, and
     so the whole pipeline may finish.
  *)
  let zip_linear2 ((z1,step1), pred, st2) =
    zip (unroll step1 z1) st2 ===
    let f (z1,z2) y =
      match step1 z1 with
      | (Some x, z1) -> (Some (x,y),(z1,z2))
      | (None, z1)   -> (None, (z1,z2))
    in
    init z1 st2 |> map_filter f 

  let zip_guard (pred,st1,st2) =
    zip (guard pred st1) st2 ===
    (zip st1 st2 |> guard (fun (z1,_) -> pred z1))

  (* A flat map can be linearized. 
     Assume that the flat-mapping function is of the form:
     fun z x -> unroll (stepi z x) (zi z x) |> guard (predi z x) |> abstract
     The state of the inner stream is thus (zp,z) where zp is eventually
     abstracted away.
     Note, in general neither step nor step1 are linear!
  *)
  let flatmap_linear2 : type a b z zp.
        (z * (z -> a option * z)) * (z -> bool) *
        ((z -> a -> zp * z -> b option * (zp * z)) *
         (z -> a -> zp * z) * (z -> a -> zp * z -> bool)) ->
        (b, z) S.stream * (b, z) S.stream =
   fun ((z,step),pred,(stepi,zi,predi)) ->
    let f z x = unroll (stepi z x) (zi z x) |> guard (predi z x) |> abstract in
    unroll step z |> guard pred |> flat_map f ===
    let z' = (None,z) in
    let rec step' = function
      | (None,z) -> begin
          match step z with
          | (_,z) when not (pred z) -> (None, (None,z))  (* exit *)
          | (None,z) -> step' (None, z)                  (* continue *)
          | (Some x,z) ->
              (* Here, stepix and predix are closures, over z and x
               *)
              let stepix zn = stepi z x zn in
              let predix zn = predi z x zn in
              let (zp,z) = zi z x in
              step' (Some (stepix, predix, zp), z)
          end
      | (Some (stepix,predix,zp),z) -> match stepix (zp,z) with
        | (_,(zp,z)) when not (predix (zp,z)) -> 
            step' (None,z)
        | (None, (zp,z)) ->
            step' (Some (stepix,predix,zp), z)
        | (Some y, (zp,z)) ->
            (Some y, (Some (stepix,predix,zp), z))
    in
    unroll step' z' |> 
    guard (fun (_,z) -> pred z) |> abstract

  (* Closures can be get rid of if we make z0 and x part of the state 
     In the real implementation, Some (x,z0,zp) are represented by three
     reference cells (uninitialized at first) and a boolean flag
   *)
  let flatmap_linear2 : type a b z zp.
        (z * (z -> a option * z)) * (z -> bool) *
        ((z -> a -> zp * z -> b option * (zp * z)) *
         (z -> a -> zp * z) * (z -> a -> zp * z -> bool)) ->
        (b, z) S.stream * (b, z) S.stream =
   fun ((z,step),pred,(stepi,zi,predi)) ->
    let f z x = unroll (stepi z x) (zi z x) |> guard (predi z x) |> abstract in
    unroll step z |> guard pred |> flat_map f ===
    let z' = (None,z) in
    let rec step' = function
      | (None,z) -> begin
          match step z with
          | (_,z) when not (pred z) -> (None, (None,z))  (* exit *)
          | (None,z) -> step' (None, z)                  (* stuttered *)
          | (Some x,z0) ->
              let (zp,z) = zi z0 x in
              step' (Some (x, z0, zp), z)
          end
      | (Some (x,z0,zp),z) -> match stepi z0 x (zp,z) with
        | (_,(zp,z)) when not (predi z0 x (zp,z)) -> 
            step' (None,z)
        | (None, (zp,z)) ->
            step' (Some (x,z0,zp), z)
        | (Some y, (zp,z)) ->
            (Some y, (Some (x,z0,zp), z))
    in
    unroll step' z' |> 
    guard (fun (_,z) -> pred z) |> abstract

end

(* Other laws to write and test

   zip_with f s1 s2 === zip_with (swap f) s2 s1
   zip_with f empty stream === zip_with f stream empty = empty
   zip_with f (singleton x) stream === map (f x) (take 1 stream)
*)

(* Overall idea: consider the term algebra of the signature stream2
   The equations induce equivalence classes of terms
   NF is a representative of an equivalence class
*)

(* A simple example of normalization: the first example of the paper
  iota C.(int 1) |> map C.(fun e -> e * e) 
  |> filter C.(fun e -> e mod (int 17) > int 7) |> take C.(int 10) 
  |> fold C.(+) C.(int 0)
*)

module NormExample1(S:stream2) = struct
  open S

  (* desugaring *)
  let stepn z = (Some z, z+1)
  let iota n = unroll stepn n

  (* satisfies guard side-conditions. In real code, again could be a ref cell *)
  let stepd x = 
    ((if x > 0 then Some x else None),x-1)

  let grd z = z >= 0
    
  let take n s = zip (unroll stepd n |> guard grd) s 
    |> map_filter (fun z (_,x) -> (Some x,z)) |> abstract

  let u1 = fun e -> e * e
  let u2 = fun e -> e mod 17 > 7

  (* original stream *)
  let s1 = 
    iota 1 
    |> map_filter (fun z x -> (Some (u1 x),z))
    |> map_filter (fun z x -> ((if u2 x then Some x else None), z))
    |> take 10

  let[@warning "-8"] [9; 16; 25; 49; 64; 81; 100; 144; 169; 196] = observe s1

  (* Fusing iota and map_filter: map_unroll *)
  let fuze_unroll_map step f = fun z -> match step z with
    | (None, z) -> (None, z)
    | (Some a,z) -> f z a

  let s2 = 
    zip 
     (unroll stepd 10 |> guard grd)
     (unroll (fuze_unroll_map 
              (fuze_unroll_map stepn (fun z x -> (Some (u1 x),z)))
              (fun z x -> ((if u2 x then Some x else None), z))) 1)
    |> map_filter (fun z (_,x) -> (Some x,z)) |> abstract

  let[@warning "-8"] [9; 16; 25; 49; 64; 81; 100; 144; 169; 196] = observe s2

  (* in s2 above, both arguments of zip are NF. In particular, the first
     argument is a LNF.
     Applying zip_guard
  *)

  let zl2 step1 = fun (z1,z2) y ->
      match step1 z1 with
      | (Some x, z1) -> (Some (x,y),(z1,z2))
      | (None, z1)   -> (None, (z1,z2))

  let s3 = 
    init 10 
     (unroll (fuze_unroll_map 
              (fuze_unroll_map stepn (fun z x -> (Some (u1 x),z)))
              (fun z x -> ((if u2 x then Some x else None), z))) 1)
    |> map_filter (zl2 stepd) 
    |> guard (fst >> grd)
    |> map_filter (fun z (_,x) -> (Some x,z)) |> abstract

  let[@warning "-8"] [9; 16; 25; 49; 64; 81; 100; 144; 169; 196] = observe s3

  (* Applying map_guard, init_unroll, map_unroll, map_unroll
     we eventually obtain the NF
  *)

  let in_step step (z1,z) = let (a,zn) = step z in (a, (z1,zn))

  let s4 = 
    unroll (fuze_unroll_map
             (fuze_unroll_map (in_step (fuze_unroll_map 
              (fuze_unroll_map stepn (fun z x -> (Some (u1 x),z)))
              (fun z x -> ((if u2 x then Some x else None), z)))) (zl2 stepd))
              (fun z (_,x) -> (Some x,z))) (10,1)
    |> guard (fst >> grd)
    |> abstract

  let[@warning "-8"] [9; 16; 25; 49; 64; 81; 100; 144; 169; 196] = observe s4

  (* If we inline fuze_unroll_map etc. *)

  let s5 = 
    unroll (fun (z1,z) ->
    if u2 (u1 z) then 
     ((if z1 > 0 then Some (u1 z) else None), (z1-1,z+1)) else (None,(z1,z+1)))
   (10,1)
    |> guard (fst >> grd)
    |> abstract

  let[@warning "-8"] [9; 16; 25; 49; 64; 81; 100; 144; 169; 196] = observe s5

end

module MN1 = NormExample1(Stream2Denot);;


(* Another, more interesting example of normalization, consider
  iota 1 |> flat_map (fun x -> from_to x (x+5)) |> take 10
*)

module NormExample(S:stream2) = struct
  open S

  (* desugaring *)
  let stepn z = (Some z, z+1)
  let iota n = unroll stepn n

  (* satisfies side-conditions. In real code, again could be a ref cell *)
  let stepnm (x,y,again) = 
    if x <= y then (Some x,(x+1,y,again)) else (None,(x,y,false))
  let from_to n m = 
    unroll stepnm (n,m,true)
    |> guard (fun (_,_,g) -> g)
    
  let take n s = zip (from_to 1 n) s 
    |> map_filter (fun z (_,x) -> (Some x,z)) |> abstract

  let swap (x,y) = (y,x)

  (* original stream *)
  let s1 = 
    iota 1 
    |> flat_map (fun z x -> from_to x (x+5) |> init z |>
                            adjust (swap,swap) |> abstract)
    |> take 10

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s1

  (* after transforming away zip *)
  let s2 = 
    iota 1 
    |> flat_map (fun z x -> from_to x (x+5) |> init z |>
                            adjust (swap,swap) |> abstract)
    |> init (1,10,true) 
    |> map_filter (fun (z1,z2) y ->
        stepnm z1 |> fun (xo,z1) -> (Option.map (fun x -> (x,y)) xo,(z1,z2)))
    |> guard (fun ((_,_,g),_) -> g)
    |> map_filter (fun z (_,x) -> (Some x,z)) |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s2

  (* intermediate result: fusing two map filters (could have done later),
     but simplifies the derivation below
   *)
  let s3 = 
    iota 1 
    |> flat_map (fun z x -> from_to x (x+5) |> init z |>
                            adjust (swap,swap) |> abstract)
    |> init (1,10,true) 
    |> map_filter (fun (z1,z2) y ->
        stepnm z1 |> fun (xo,z1) -> (Option.map (fun _ -> y) xo,(z1,z2)))
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s3

  (* moving init, fusing with unroll *)
  let combi u = fun (zp,z) -> match u z with (xo,z) -> (xo,(zp,z))
  let s4 = 
    unroll (combi stepn) ((1,10,true),1)
    |> flat_map (fun (zp,z) x -> from_to x (x+5) |> init z |>
      adjust (swap,swap) |> abstract |> init zp)
    |> map_filter (fun (z1,z2) y ->
        stepnm z1 |> fun (xo,z1) -> (Option.map (fun _ -> y) xo,(z1,z2)))
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s4

   (* let _ = observe ~limit:14 s4 |> List.iter (Printf.printf "%d\n") *)

  (* moving map_filter into flatmap *)
  let s5 = 
    unroll (combi stepn) ((1,10,true),1)
    |> flat_map (fun (zp,z) x -> from_to x (x+5) |> init z
        |> adjust (swap,swap) |> abstract |> init zp
        |> map_filter (fun (z1,z2) y ->
             stepnm z1 |> fun (xo,z1) -> (Option.map (fun _ -> y) xo,(z1,z2))))
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s5

  (* merging init z with from_to *)
  let s6 = 
    unroll (combi stepn) ((1,10,true),1)
    |> flat_map (fun (zp,z) x -> 
        unroll (combi stepnm) (z,(x,x+5,true)) |> guard (fun (_,(_,_,g))->g)
        |> adjust (swap,swap) |> abstract |> init zp
        |> map_filter (fun (z1,z2) y ->
             stepnm z1 |> fun (xo,z1) -> (Option.map (fun _ -> y) xo,(z1,z2))))
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s6

  let adj_z1z2_z = ((fun (z1,(z2,z)) -> (z,(z1,z2))),
                    (fun (z,(z1,z2)) -> (z1,(z2,z))))

  (* merging init zp with from_to *)
  let s7 = 
    unroll (combi stepn) ((1,10,true),1)
    |> flat_map (fun (zp,z) x -> 
        unroll (combi (combi stepnm)) (zp,(z,(x,x+5,true))) 
        |> guard (fun (_,(_,(_,_,g)))->g)
        |> adjust adj_z1z2_z |> abstract 
        |> map_filter (fun (z1,z2) y ->
             stepnm z1 |> fun (xo,z1) -> (Option.map (fun _ -> y) xo,(z1,z2))))
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s7

  (* moving map_filter closer to unroll *)
  let s8 = 
    unroll (combi stepn) ((1,10,true),1)
    |> flat_map (fun (zp,z) x -> 
        unroll (combi (combi stepnm)) (zp,(z,(x,x+5,true))) 
        |> guard (fun (_,(_,(_,_,g)))->g)
        |> map_filter (fun (z1,(z2,z)) y ->
           stepnm z1 |> fun (xo,z1) -> (Option.map (fun _ -> y) xo,(z1,(z2,z))))
        |> adjust adj_z1z2_z |> abstract)
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s8

  (* more closer; later merging with unroll *)
  let s9 = 
    unroll (combi stepn) ((1,10,true),1)
    |> flat_map (fun (zp,z) x -> 
        unroll (combi (combi stepnm)) (zp,(z,(x,x+5,true))) 
        |> map_filter (fun (z1,(z2,z)) y ->
           stepnm z1 |> fun (xo,z1) -> (Option.map (fun _ -> y) xo,(z1,(z2,z))))
        |> guard (fun (_,(_,(_,_,g)))->g)
        |> adjust adj_z1z2_z |> abstract)
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s9

  (* normal form *)
  let combii u f = fun z -> match u z with
  | (None,z)   -> (None,z)
  | (Some x,z) -> f z x
  let s10 = 
    unroll (combi stepn) ((1,10,true),1)
    |> flat_map (fun (zp,z) x -> 
        unroll (combii (combi (combi stepnm))  
           (fun (z1,(z2,z)) y ->
           stepnm z1 |> fun (xo,z1) -> 
             (Option.map (fun _ -> y) xo,(z1,(z2,z)))))
          (zp,(z,(x,x+5,true)))
        |> guard (fun (_,(_,(_,_,g)))->g)
        |> adjust adj_z1z2_z |> abstract)
    |> guard (fun ((_,_,g),_) -> g)
    |> abstract

  let[@warning "-8"] [1; 2; 3; 4; 5; 6; 2; 3; 4; 5] = observe s10
end



module MN = NormExample(Stream2Denot)

;;

