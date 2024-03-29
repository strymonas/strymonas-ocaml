(*  Raw streams: 'a stream

   It is the representation of a stream processing pipeline: a producer
   and a sequence of transformers. A consumer such as fold (iter) 
   interprets this representation and genereates loops.
   Type parameter 'a is the type of the eventually produced value. 
   For raw streams in this file, is not necessarily 
   the code type! It could be a tuple of code types, for example. It could
   even be a base type, for a constant stream, although this is rarely
   useful.

   After many, many experiments, we eventually found it best to separate
   out the termination condition. We also consider state mutable and encapsulate
   rather than pass around explicitly. It is also better to write the
   element producer in CPS: we can do let-insertion, and also we can
   fail to invoke continuation when the stream is really finished.
   The latter accomodates the situation when deciding if a stream is finished
   involves trying to get a new element (which is the case in many I/O
   operations). The explicit termination condition permits fusing (conjoining)
   the conditions and generating better code (especially in cases like
   accessing arrays, where the range check can be made before trying to
   access an element). Also, explicit breaking out is indispensable in zipping.

   Of stream producers, we distinguish indexing producers, which are what
   some call `pull array'. They correspond
   to the for-loop. The general unfold producer corresponds to the while
   loop (or the tail-recursive function).
   The loop itself will be generated by iter (as behooves to pull
   streams). We assume that the iter's state is mutable, so we make no 
   provisions to thread it through in the generator.

   See derivation.ml for all derivation and justifications.
 *)

(* Utilities *)

(* Function composition *)
let (<<) f g = fun x -> f (g x)
let (>>) f g = fun x -> f x |> g

(* The identity function that makes CPS convenient *)
let (let-) c k = c k

module type cde        = module type of Cde
module type stream_raw = module type of Stream_raw

module Make(C: cde) : (stream_raw with type 'a exp = 'a C.exp and
                                       type 'a stm = 'a C.stm and
                                       type 'a mut = 'a C.mut and
                                       type 'a arr = 'a C.arr and
                                       type 'a tbase = 'a C.tbase) = struct

(* Open C only locally; otherwise its (&&) etc. operators interfere.
   Local opens also makes the code generation places explicit
*)
(*
module C = (Trx_code : (cde with type 'a cde = 'a code))
*)
type 'a exp   = 'a C.exp 
type 'a stm   = 'a C.stm 
type 'a mut   = 'a C.mut 
type 'a arr   = 'a C.arr 
type 'a tbase = 'a C.tbase

(* Producer of a value. When evaluated repeatedly, may produce different values.
   This is the primitive of initalizers and loops.
   The received continuation must be invoked at most once.
   If the continuation is not invoked, we assume that the producer
   has `stuttered' (advanced the state but failed to produce any element).
   Termination condition is dealt with separately and is not part of
   emit.
*)
type 'a emit = ('a -> unit stm) -> unit stm

let fmap_emit : ('a -> 'b emit) -> 'a emit -> 'b emit = fun f s ->
  fun k -> s (fun x -> f x k)

(* Another primitive producer: a pull array *)
type 'a pull_array = {upe: int exp;      (* upper bound: exclusive: length *)
                      index: int exp -> 'a emit}

let fmap_pa : ('a -> 'b emit) -> 'a pull_array -> 'b pull_array = 
  fun f ({index} as g) -> {g with index = index >> fmap_emit f}


type ('a,'b) eq = Refl : ('a,'a) eq

(* The type of the initializer *)
type 'a init =
  (* Essentially let-insertion. The initializing expression may be stateful *)
  | Ilet : 'a exp -> 'a exp init
  (* Mutable state with the given initial value 
     Eq is a hack: without it, OCaml compiants that it can't figure out the
     type of 'a: because 'a exp is abstract
   *)    
  | Iref : 'a exp * ('a mut,'b) eq -> 'b init
  (* Mutable state, not initialized.
     The given value is merely a hint to the backend about the
     type of the value. The back end doesn't have to initialize the value,
     or initialize it with the given value (it could use a constant of
     an appropriate type).
     It is the responsibility of this module to assign to this cell
     before dereferencing it.
     Eq is a hack: without it, OCaml compiants that it can't figure out the
     type of 'a: because 'a exp is abstract
   *)    
  | Iuref : 'a exp * ('a mut,'b) eq -> 'b init
  (* An *non-empty*
     array with a statically known, and preferably rather small size.
     Generally mutable *)
  | Iarr : 'a exp array * 'a C.tbase * ('a arr,'b) eq -> 'b init
  (* An uninitialized array of the given size. The second argument is
     is a type descriptor.
  *)
  | Iuarr : int * 'a C.tbase * ('a arr,'b) eq -> 'b init
  (* A *non-empty* array with a statically known content.
     It should not be mutated *)
  | Istarr : 'a C.tbase * ('c-> 'a C.exp) * 'c array * ('a arr,'b) eq -> 'b init


(* The termination condition: true to continue, false to terminate the stream.
   It should be idempotent and cheap to evaluate
   XXX Perhaps I should use GExp of bool exp list, to later ensure
   right association of conjunctions?
   XXX Perhaps treat GRef as a sort of global exit.
   So, init (GRef x) is like allocation of the label,
   and assignment to x false is like goto.
   Make for_ and while_ take an extra argument: global exits.
   For OCaml generator, these exits are GRef that are ANDed with the
   termination condition. For C code generator, we just ignore these
   GRefs and use goto whenever it is assigned false.
 *)
type goon = 
  | GTrue                               (* constant true *)
  | GExp of bool exp                    (* should be cheap to evaluate *)
  | GRef of bool mut                    (* a boolean flag *)

let exp_of_goon : goon -> bool exp = function
  | GTrue  -> C.bool true
  | GExp x -> x
  | GRef x -> C.dref x

let goon_conj : goon -> goon -> goon = fun g1 g2 ->
  match (g1,g2) with
  | (GTrue,g) | (g,GTrue) -> g
  | (GExp g1, GExp g2) -> GExp C.(g1 && g2)
  | (GRef g1, GRef g2) -> GExp C.(dref g1 && dref g2)
  | (GRef g1, GExp g2) -> GExp C.(dref g1 && g2)
  | (GExp g1, GRef g2) -> GExp C.(dref g2 && g1) (* deref is cheaper *)

let goon_disj : goon -> goon -> goon = fun g1 g2 ->
  match (g1,g2) with
  | (GTrue,_) | (_,GTrue) -> GTrue
  | (GExp g1, GExp g2) -> GExp C.(g1 || g2)
  | (GRef g1, GRef g2) -> GExp C.(dref g1 || dref g2)
  | (GRef g1, GExp g2) -> GExp C.(dref g1 || g2)
  | (GExp g1, GRef g2) -> GExp C.(dref g2 || g1) (* deref is cheaper *)

(* Stream itself *)

type 'a producer = 
  | For    : 'a pull_array -> 'a producer
  | Unroll : 'a emit       -> 'a producer

(* A stream is called linear if its producer never stutters (that is,
   always produces an element when called) -- or, if stutters,
   only at the end. That is, if a linear producer stutters, it never
   again produces an element.
   Filtered accumulates a list of predicates. We use a list for
   accumulation to ensure that when we eventually conjoin the predicates,
   they will be associated to the right. Left-association is inefficient
   (see our reflection-without-remorse)
 *)
type 'a linearity =
  | Linear
  | Nonlinear
  | Filtered of ('a -> bool exp) list

  (* goon signals early termination
     Consider 'a emit as throwing an exception (`break', in C terms)
     to signal finish, and goon catching it.
   *)
type 'a flat = 'a linearity * goon * 'a producer

type 'a stream = 
    (* Initializer 
       It should produce code (just like in the case of Nested)
     *)    
  | Init  : 's init * ('s -> 'a stream) -> 'a stream
  | Flat  : 'a flat -> 'a stream
  (* The stream to map on must produce 'b exp rather than
     any type. It becomes important when we linearize the stream.
   *)
  | Nested : goon * 'b exp flat * ('b exp -> 'a stream) -> 'a stream

(* Start the main code *)

(* It is a sort of a flat_map *)
let initializing : 'z exp -> ('z exp -> 'a stream) -> 'a stream =
  fun init sk -> 
    if C.is_static init then sk init
    else if C.is_fully_dynamic init then Init (Ilet init,sk)
    else sk (C.glet init)

let initializing_ref : 
    'z exp -> ('z mut -> 'a stream) -> 'a stream =
    fun init sk -> Init (Iref (init,Refl),sk)
(* Non exported: used only internally. It's too confusing to others.
   initializing_ref is preferred. *)
let initializing_uref : 
    'z exp -> ('z mut -> 'a stream) -> 'a stream =
    fun init sk -> Init (Iuref (init,Refl),sk)
let initializing_arr : 
    'z C.tbase -> 'z exp array -> ('z arr -> 'a stream) -> 'a stream =
    fun tb init sk -> Init (Iarr (init,tb,Refl),sk)
let initializing_uarr : 
    'z C.tbase -> int -> ('z arr -> 'a stream) -> 'a stream =
    fun tb size sk -> Init (Iuarr (size,tb,Refl),sk)
let initializing_static_arr  : 'z tbase -> ('b -> 'z exp) -> 'b array -> 
                       ('z arr -> 'a stream) -> 'a stream =
    fun tb cnv arr sk -> Init (Istarr (tb,cnv,arr,Refl),sk)



let pull_array : int exp -> (int exp -> 'a emit) -> 'a stream = 
  fun upe idx -> 
    Flat (Linear, GTrue, For {upe; index = (fun i k -> idx i k)})

(* Change the For producer to the general producer
   Keep in mind that after we fuse filter, index is no longer linear!
*)
let for_unfold : 'a flat -> 'a stream = function 
  | (_,_,Unroll _) as sf -> Flat sf
  | (m,g,For {upe;index}) ->
      let- i = initializing_ref (C.int 0) in
      Flat (m, goon_conj g (GExp C.(dref i < upe)),
               Unroll (fun k -> C.(seq (index (dref i) k) (incr i))))

(* Create an infinite stream: run step in an infinite loop 
   PRE: 'a emit is supposed to be linear
*)
let infinite : 'a emit -> 'a stream = fun step -> 
  Flat (Linear, GTrue, Unroll step)

let rec guard : goon -> 'a stream -> 'a stream = fun g -> function
  | Init (init,sk)        -> Init (init, sk >> guard g)
  | Flat (m,g',p)         -> Flat (m, goon_conj g' g, p)
  | Nested (g',st,next)   -> Nested (goon_conj g' g, st, next)


(* A general map, used for many things
   It is guaranteed to be applied sequentially. So, it is actually map_accum
*)

(* fmap for the option type, mapper is in CPS *)
let fmap_option_cps :
    ('a -> ('b -> 'w) -> 'w) ->
    ('a option -> ('b option -> 'w) -> 'w) = fun f e k ->
      match e with
      | None -> k None
      | Some x -> f x (fun y -> k (Some y))

(* The transformer is not necessarily linear *)
let map_producer : ('a -> 'b emit) -> 'a producer -> 'b producer = fun tr -> 
  function
    | For pa    -> For (fmap_pa tr pa)
    | Unroll st -> Unroll (fmap_emit tr st)

(* fold-in filtered *)
   (* ensure right-associativity! The list is assumed non-empty *)
let conjoin_preds : ('a -> bool exp) list -> ('a -> bool exp) = fun l ->
  fun x ->
    let rec loop = function
      | []   -> assert false
      | [p]  -> p x
      | h::t -> C.(h x && loop t)
    in loop l 

let normalize_flat : 'a flat -> 'a flat = function
  | (Filtered preds, g, p) ->
      let pred = conjoin_preds preds in
      (Nonlinear, g, map_producer (fun x k -> C.if1 (pred x) (k x)) p)
  | x -> x

    (* We need the mapping function in CPS with the code answer type,
       so we can do let-insertion
       We also define map_raw' with the `direct-style' mapper

       The optional argument, ?linear, tells if the transformer is
       linear -- that is, if the continuation (b -> unit stm) is invoked
       exactly once. 
       By default it is true.
       If the continuation ends up not being invoked in some cases
       (that is, map_raw behaves like map_option), be sure specify
       ~linear:false!
       On no occasion should continuation be invoked multiple times
       to keep the iteration state in sync with the loop's guard condition!
   *)
let rec map_raw : type a b. ?linear:bool ->
   (a -> b emit) -> a stream -> b stream =
   fun ?(linear=true) tr -> function
     | Init (init, sk)    -> Init (init, sk >> map_raw ~linear tr)
     | Flat ((Filtered _,_,_) as sf) ->
         Flat (normalize_flat sf) |> map_raw ~linear tr
     | Flat (Linear,g,p)     -> 
         Flat ((if linear then Linear else Nonlinear),g, map_producer tr p)
     | Flat (Nonlinear,g,p)  -> Flat (Nonlinear,g, map_producer tr p)
     | Nested (g,st,next)    -> Nested (g, st, map_raw ~linear tr << next)
 and map_raw' : type a b. (a -> b) -> a stream -> b stream =
   fun f st -> map_raw (fun e k -> k (f e)) st

(* Consumer. Only consumer runs the main loop -- or makes it *)
(* The function loop below implements generating loop with `break' like
   in C (or better, Perl or WASM) where one could break out of
   the block. OCaml does not support such loops, so we have to resort
   to auxiliary boolean terminator flags. The break operator sets the 
   flag to false.
   A breakpoint is a condition which, if evaluates to false, terminates
   all loops in its scope. The breakpoint is usually triggered (that is,
   state on which it depends is modified) when evaluating the producer 
   (or, sometimes, consumer) code.
   For nested loops, the breakpoint extends to the nested loops: when the
   outer loops is terminated, the inner loops should also finish.

   Indeed, consider the pipeline
   stout |> flat_map inner |> take n
   When the requested number of elements is taken, both the outer stream
   stout and the inner stream should terminate.
   The exit from the inner loop means that the outer loop also terminates.
   Thus we effectively enhance the state of the outer stream to deal
   with such deep exits.
*)

let iter : ('a -> unit stm) -> 'a stream -> unit stm
 = fun consumer st ->
   let rec consume : type a. goon -> (a->unit stm) -> a producer -> unit stm = 
     fun g consumer -> function
     | For {upe;index} ->
         let bp = if g = GTrue then None else Some (exp_of_goon g) in
         C.for_ C.(int 0) ~upe ?guard:bp (fun i -> index i consumer)
     | Unroll step ->
         C.(while_ (exp_of_goon g) (step consumer))

    and loop : type a. (a -> unit stm) -> a stream -> unit stm =
     fun consumer -> function
    | Init (Ilet i, sk) ->
        C.letl i @@ fun z -> loop consumer (sk z)
    | Init (Iref (i,Refl), sk) -> 
        C.newref i @@ fun z -> loop consumer (sk z)
    | Init (Iuref (i,Refl), sk) ->
        C.newuref i @@ fun z -> loop consumer (sk z)
    | Init (Iarr (a,tb,Refl), sk) ->
        C.new_array tb a @@ fun z -> loop consumer (sk z)
    | Init (Iuarr (n,tb,Refl), sk) ->
        C.new_uarray tb n @@ fun z -> loop consumer (sk z)
    | Init (Istarr (tb,cnv,arr,Refl), sk) ->
        C.new_static_array tb cnv arr @@ fun z -> loop consumer (sk z)

    | Flat (Filtered preds, g, st) -> 
         let pred = conjoin_preds preds in
         let consumer' x = C.if1 (pred x) (consumer x) in
         consume g consumer' st
    | Flat (_, g, st)    -> consume g consumer st
      (* The inner stream inherits the breakpoints for the outer stream:
         the termination conditions for the outer stream should also
         terminate inner streams
         XXX Perhaps, instead of duplicating g I should allocate
         a termination flag
       *)           
    | Nested (g, st, next) ->
        loop (loop consumer << guard g << next) (guard g (Flat st))
   in
    loop consumer st

(* Other Transformers *)

(* Apply the filter laws here *)
let rec filter_raw : type a. (a -> bool exp) -> a stream -> a stream =
   fun pred -> function
     | Init (init,sk)  -> Init (init, filter_raw pred << sk)
     | Flat (Filtered p', g, p) ->
         Flat (Filtered (p' @ [pred]), g, p)
     | Flat (_, g, p) -> Flat (Filtered [pred], g, p)
     | Nested (g,s,next) -> Nested (g,s,filter_raw pred << next)


let rec flat_map_raw : type a b.
      (a exp -> b stream) -> a exp stream -> b stream =
   fun next -> function
     | Init (init,sk)       -> Init (init, flat_map_raw next << sk)
     | Flat sf              -> Nested (GTrue, sf, next)
     | Nested (g,sf, next') -> Nested (g,sf, flat_map_raw next << next')


(* Zipping *)
(* When zipping two streams with the step function step1 and step2
   (and assuming step1 is at least as long as step2), one may expect
   one of the following patterns of calling step1 and step2:
     step1 step2 step1 step2 step1 eof-step2
   or
     step2 step1 step2 step1 eof-step2
   We guarantee that step1 and step2 are called in the alternating
   pattern: one of the above calling sequences. Which of the two -- is
   generally undefined. The programmer has to beware, when step functions
   have side-effects.

   Care should be taken when zipping the nested stream with an
   ordinary stream.
   Another subtle point: we try to make a parallel loop, so to speak.
   However, the two streams may advance at different pace: they
   may skip at different times. So, if one stream yields an element
   and another skips, we should `back up' the element of the first
   stream. Since the state is imperative, we can only remember the
   element in the putback buffer. This way we make the streams advance
   at the same speed.

   XXX Keep in mind the complex cases of zipping:
      ofArr ... |> (fun x -> ofArr [|x;x+1;x+2|])
      ofArr ... |> (fun x -> ofArr [|x;x+2|])
   inner streams have different sizes; one one finishes, the second
   still has some elements, which should not be lost!
*)


(* Zipping of two producer streams, ASSUMING they are both linear *)
let zip_emit : 'a emit -> 'b emit -> ('a*'b) emit = fun i1 i2 ->
  fun k -> i1 (fun x -> i2 (fun y -> k (x,y)))

let zip_pull_array : 'a pull_array -> 'b pull_array -> ('a*'b) pull_array =
  fun p1 p2 ->
   {upe  = C.imin (p1.upe) (p2.upe);
   index = (fun i -> zip_emit (p1.index i)  (p2.index i))}

(* Linearization *)
(* The most complex is linearization of a nested stream, such as the following
   typical stream

   Nested(gout, Flat (g, Unroll step), fun x ->
       Init (i', fun z -> Flat(g', Unroll step')))

   That is, the while-loop with the go-on condition g and the body
   step as the outer stream, and the while-loop with the initialization
   i', go-on condition g' and the body step' as the inner stream. All
   components of the inner stream may depend on x. Since x is a code
   value, we can allocate a ref cell and make this code value be the
   code reading from the cell. This emulates closure-conversion. We
   deal with z likewise. In the following, i', g' and step' are such
   `closure-converted' code (where x and z are substituted with the
   code to read from the appropriate reference cells).

   As we explained in iter before, the outer guard gout is distributed to
   both g and g'. Below we assume it has been distributed such way.

   The linearized stream is also a while-loop, with the guard
   goon (a boolean ref cell with the initial value true) and the
   following step. There, in_inner is a boolean
   flag telling that the inner stream is advancing. It is a reference cell
   with the initial value false and to be allocated outside the loop, 
   in the init section.  When the inner loop is initialized, in_inner is
   its goon condition. Also allocated outside the loop are the ref 
   cell xres to hold the result of the outer loop and zres to hold
   the result of i'.

   (See also flatmap_linear2 in derivation.ml)

   fun k ->
   let again = ref true in  (* C-like loop with `continue' *)
   while !again do
   if not !in_inner then begin (* The inner loop is finished or hasn't started*)
     if g then
       step (fun x -> xres := x;
                      zres := i';   (* init the closures of the inner loop *)
                      in_inner := g')
     else goon := false    (* The inner loop was finished, and now, the outer
                              loop as well. So we are done *)
       (* if step stuttered, in_inner is still false and again is still true,
          so we `continue' *)
   end
   else ();
   if !in_inner then     (* step' is not yet finished *)
       step' (fun y -> again := false; k y);
       in_inner := g';
         (* if step' stuttered, again is still true and we `continue' *)
    else ();
    if !again then !again := !goon
  done

Even if the outer and inner streams are linear (not stuttering), 
we may still need the while-again loop: the g' condition of the inner loop
may be true but step' didn't call its continuation because it tried
but failed to get the element (g' becomes false). In that case, we have
to re-execute the outer loop.
The simplification is basically only possible when the inner loop is
a for-loop with the static upper bound. Then we can tell before executing
step if it is going to produce an element or not.

For a deeply nested stream, we don't need to linearize inner streams,
just to unnest them. 
*)

(* Estimate, how hard it is to linearize a stream. Return a non-negative
   number, the bigger it is, the harder is linearization (more code is
   generated)
   Return value 0 means the stream is already linear
 *)
let linearize_score : type a. a stream -> int =  fun st ->
  let cnt = ref 0 in
  let add x = cnt := !cnt + x in
  let rec loop : type a. a stream -> unit = function
  | Init (Ilet i, sk)         -> loop (sk i)
  | Init (Iref (i,Refl), sk)  -> 
      ignore (C.newuref i @@ fun z -> loop (sk z); C.unit)
  | Init (Iuref (i,Refl), sk) ->
      ignore (C.newuref i @@ fun z -> loop (sk z); C.unit)
  | Init (Iarr (a,tb,Refl), sk)  ->
      ignore (C.new_uarray tb 0 @@ fun z -> loop (sk z); C.unit)
  | Init (Iuarr (_,tb,Refl), sk)->
      ignore (C.new_uarray tb 0 @@ fun z -> loop (sk z); C.unit)
  | Init (Istarr (tb,_,_,Refl), sk)->
      ignore (C.new_uarray tb 0 @@ fun z -> loop (sk z); C.unit)
  | Flat (Linear,_,_) -> ()
  | Flat _            -> add 3
  | Nested (_,s,sk)   -> add 5; loop (Flat s);
      match s with
      | (_,_,For {index}) -> 
          ignore (index C.(int 0) (fun e -> loop (sk e); C.unit))
      | (_,_,Unroll st) ->
          ignore (st (fun e -> loop (sk e); C.unit))
  in loop st; !cnt

(* The result may have more Init's added
   This is a variation of the iter, in a sense.
   We replace all For producers with Unfold (at the beginning, actually)
*)
let linearize : type a. a stream -> a stream = fun st ->
  (* linearize a non-linear flat stream

    let again = ref true in
    while !again do
      step (fun x -> again := false; k x)
      if !again then !again := g
    end
   *)
  let rec loopnn: type a. a flat -> a stream = function
    | (Linear,_,_) as sf     -> Flat sf
    | (Filtered _,_,_) as sf -> normalize_flat sf |> loopnn
    | (Nonlinear, g, For _)  -> assert false
    | (Nonlinear, g, Unroll s) ->
        let bp = if g = GTrue then None else Some (exp_of_goon g) in
        Flat (Linear, g, Unroll (fun k -> C.(cloop k bp s)))
  in
  (* linearize a nested stream *)
  (* Normalize the outer stream of the nested stream
     We follow the code explained earlier
   *)
  let rec nested : type a b. 
    goon -> (b exp -> a stream) -> b exp flat -> a stream = 
    fun gouter next -> function
    | (Filtered _,_,_) -> assert false
    | (_,_,For _)      -> assert false
    | (_,g,Unroll step) ->
    (* Guess a sample value produced by step -- or just its type *)
    let outer_sample : b exp option =
      let r = ref None in
      ignore (step (fun x -> (if !r = None then r := Some x); C.unit)); !r
    in match outer_sample with
      (* If step never calls its continuation, it is an empty stream *)
    | None -> Flat (Linear, GTrue, Unroll (fun k -> C.unit))
    | Some outer_sample ->
    (* First, create initialization and guards *)
    let g = goon_conj gouter g in
    (* XXX If g turns out to be a GRef already, don't allocate gref,
       use it as it is. We don't need gref := g below either. *)
    initializing_ref C.(bool true) @@ fun goon ->
    initializing_ref C.(bool false) @@ fun in_inner ->
    let guard = GRef goon  in
    initializing_uref outer_sample @@ fun xres ->
          (* The recursive linearization may create another while loop,
             if the inner stream is non-linear.
             At the very least, we merely need to unnest the inner loop.
             It may well be non-linear
          *)
    let st' = main true (next (C.dref xres)) in
    split_init C.unit st' @@ fun i' (g',step') ->
    let g' = goon_conj gouter g' in
    Flat (Linear, guard,
      Unroll (fun k ->
        C.(newref (bool true) @@ fun again ->
           while_ (dref again) @@
            seq
              (if1 (not (dref in_inner))
                 (if_ (exp_of_goon g)
                   (step (fun x -> seq (xres := x)
                                   (seq i'
                                     (in_inner := exp_of_goon g'))))
                   (seq (goon := bool false) (again := bool false))))
              (if1 (dref in_inner) @@
                 (seq (step' (fun x -> seq (k x) (again := bool false))) 
                      (in_inner := exp_of_goon g')))
             )
          ))

            (* split off the initialization code and 
               return a `closure-converted stream' *)
  and split_init : 
    type a w. unit stm -> a stream -> 
              (unit stm -> (goon * a emit) -> w stream) -> w stream = 
    fun init st k -> match st with
      (* If it can be lifted, doesn't need to be closure-converted *)
    | Init (Ilet i, sk) when not (C.is_fully_dynamic i) ->
      initializing i @@ fun zres ->
      split_init init (sk zres) k
    | Init (Ilet i, sk) ->
      initializing_uref i @@ fun zres ->
      split_init C.(seq init (zres := i)) (sk (C.dref zres)) k
    | Init (Iref (i,Refl), sk) -> 
      initializing_uref i @@ fun zres ->
      split_init C.(seq init (zres := i)) (sk zres) k
    | Init (Iuref (i,Refl), sk) -> 
      initializing_uref i @@ fun zres ->
      split_init init (sk zres) k
    | Init (Iarr (a,tb,Refl), sk) -> 
      initializing_uarr tb (Array.length a) @@ fun zres ->
      split_init 
          (let rec loop i acc =
            if i >= Array.length a then acc else
            loop (i+1) C.(seq acc (array_set zres (int i) a.(i)))
           in loop 0 init)
          (sk zres) k
    | Init (Iuarr (n,t,Refl), sk) -> 
      initializing_uarr t n @@ fun zres ->
      split_init init (sk zres) k
    | Init (Istarr (t,cvn,arr,Refl), sk) -> 
      initializing_static_arr t cvn arr @@ fun zres ->
      split_init init (sk zres) k
    | Flat (Filtered _,_,_)  -> assert false
    | Flat (_,_,For _)       -> assert false
    | Flat (_,g,Unroll step) -> k init (g,step)
    | Nested _ -> failwith "Inner stream must be flattened first"
  and
    (* The first argument tells if unnesting is OK, no need
       to do the full linearization
     *)
    main : type a. bool -> a stream -> a stream = fun unn -> function
    | Init (init, sk)            -> Init (init, main unn << sk)
    | Flat ((_,_,For _) as sf)   -> main unn (for_unfold sf)
    | Flat sf when unn           -> Flat (normalize_flat sf)
    | Flat sf                    -> loopnn sf
    | Nested (g, ((_,_,For _) as sf), next) ->
        for_unfold sf |> flat_map_raw next |> guard g |> main unn
    | Nested (g, ((Filtered _, _,_) as sf), next) ->
        Nested (g, normalize_flat sf, next) |> main unn
    | Nested (g,sf,next) -> nested g next sf
  in main false st

(* The dispatcher for zip *)

let rec zip_raw : type a b. a stream -> b stream -> (a * b) stream =
  fun st1 st2 ->
   let swap st = map_raw' (fun (x,y) -> (y,x)) st in
   match (st1,st2) with
   | (Init (init, sk),st2)  -> Init (init, fun z -> zip_raw (sk z) st2)
   | (st1,Init (init, sk))  -> Init (init, fun z -> zip_raw st1 (sk z))

   | (Flat (Linear,g1,For pa1), Flat (Linear,g2,For pa2)) ->
       Flat (Linear, goon_conj g1 g2, For (zip_pull_array pa1 pa2))
   | (Flat (Linear,g1,Unroll s1), Flat (Linear,g2,Unroll s2)) ->
       Flat (Linear, goon_conj g1 g2, Unroll (zip_emit s1 s2))

   (* zipping with a stream that is linear *)
   | (Flat (Linear, g, Unroll s), st2) -> 
       guard g @@ map_raw (fun y k -> s (fun x -> k (x,y))) st2
   | (_, Flat (Linear,_,_)) -> zip_raw st2 st1 |> swap  

   (* If there are linear For remaining, convert to Unfold *)
   | (Flat ((Linear,_,For _) as sf1), st2) -> zip_raw (for_unfold sf1) st2


   (* If both streams are non-linear, make one of them linear *)
   | (st1, st2) -> 
       if linearize_score st2 > linearize_score st1
       then zip_raw (linearize st1) st2 else zip_raw st1 (linearize st2)
end
;;
