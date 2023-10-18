(* Flexible window processing, mainly for the sake of
   digital signal processing (filtering, etc.)

  Window processing is uniform in the backend: that's why it is a functor
*)

(* The identity function that makes CPS convenient *)
let (let-) c k = c k

module type cde_ex     = module type of Cde_ex
module type stream_raw = module type of Stream_raw

module Make(C: cde_ex)(Raw: stream_raw with 
                          type 'a exp = 'a C.exp and
                          type 'a stm = 'a C.stm and
                          type 'a mut = 'a C.mut and
                          type 'a arr = 'a C.arr and
                          type 'a tbase = 'a C.tbase) = struct

(* Open C only locally; otherwise its && etc. operators interfere 
   Local opens also makes the code generation places explicit
*)
type 'a exp   = 'a C.exp 
type 'a stm   = 'a C.stm 
type 'a mut   = 'a C.mut 
type 'a arr   = 'a C.arr 
type 'a tbase = 'a C.tbase
type 'a stream  = 'a Raw.stream
type 'a cstream = 'a exp stream
open Raw

(* Note on performance
   A key to an easily vectorizable code is to generate loops such that:
   - boundaries are static or at least simple
   - indexing is simple: a[i] is simple, a[i+1] may be too,
     but a[c-i] is not
   As another constraint, since the current element is available immediately,
   it is better to write the reduction as
   x[i] `op` x[i-1] ... `op` [x-m+1]
   (The operation op may not have a unit),
   so that the current element x[i] can be used to initialize the accumulator.
   Therefore, to make sure the history buffer is accessed as just buffer[i]
   where i is the loop index (in the order of the loop index),
   the buffer should be filled *backwards*: the most recent element should have
   the smallest index.
   *)
module type window = sig
  type e                                (* element type: base type *)
  type t                                (* window type *)
  val etyp : e tbase
  val size : int                        (* statically known, >0 *)
  val make_stream : ?padding:bool -> e exp stream -> t stream

  (* General weghted reduction of the window: essentially map-reduce
       reduce_gen op extr
     The operation extr i e is mapping: maps the i-th element e of the window
     to a value domain a, and op is an associative operation on that domain.
   *)

  val reduce_gen : ('a exp -> 'a exp -> 'a exp) -> 
      (int exp -> e exp -> 'a exp) -> t -> ('a exp -> 'w stm) -> 'w stm
end

(* introduce two convenient specializations of reduce_gen *)
module type window_ex = sig
   include window

    (* reduce the window with an associative operation
       When used as an argument of map_raw, effectively computes
       signal[i] `op` signal[i-1] ... `op` signal[i-size+1]
    *)
    val reduce : (e exp -> e exp -> e exp) -> t -> 
      (e exp -> 'w stm) -> 'w stm

    (* 
       When used as an argument of map_raw, effectively computes the
       convolution 
       weight[0] * signal[i] +
       weight[1] * signal[i-1] + ...
       weight[size-1] * signal[i-size+1]
     where + stands for the reduction op.
     *)
                                      (* array of weights, length = size *)
    val dot : 'a tbase -> ('b -> 'a exp) -> 'b array ->
      (e exp -> e exp -> e exp) ->     (* reduction op, associative *)
      ('a exp -> e exp -> e exp) ->    (* weight `multiplication' *)
      t -> (e exp -> 'w stm) -> 'w stm
end

(* The extended operations can often be computed generically *)
module WinEx(W:window) = struct
  include W
   
    let reduce : (e exp -> e exp -> e exp) -> t -> 
      (e exp -> 'w stm) -> 'w stm = fun op t k -> 
          reduce_gen op (fun _ x -> x) t k

    let dot : 'a tbase -> ('b -> 'a exp) -> 'b array ->
      (e exp -> e exp -> e exp) ->     (* reduction op, associative *)
      ('a exp -> e exp -> e exp) ->    (* weight `multiplication' *)
      t -> (e exp -> 'w stm) -> 'w stm = 
    fun typ lift weights op mul t k ->
      assert (Array.length weights = size);
      let open C in
      new_static_array typ lift weights @@ fun wt ->
      reduce_gen op (fun i x -> mul (array_get' wt i) x) t k
end

type 'a window = (module window_ex with type e = 'a)

  (* makers of windows in various particular cases *)

  (* The simplest window, of size 2. If slide = true, it slides by one,
     otherwise, tumbles
  *)
module Size2(S: sig type e val etyp: e tbase val slide: bool end) = struct
  type e = S.e
  let etyp = S.etyp
  type t = {prev: e mut; curr: e exp}
  let size = 2

  let make_stream : ?padding:bool -> e exp stream -> t stream = 
    fun ?(padding=false) st ->
      if padding && S.slide then
        (* If the window is sliding and initially padded by 0,
           then it is always fully filled and we don't need inited.
        *)
      let- prev   = Raw.initializing_ref (C.tbase_zero etyp) in
      st |>
      Raw.map_raw C.(fun e k -> k {prev; curr=e} @. (prev := e))
      else (* general case *)
      let- inited = Raw.initializing_ref (C.bool padding) in
      let- prev   = Raw.initializing_ref (C.tbase_zero etyp) in
      st |>
      Raw.map_raw ~linear:false C.(fun e k ->
        if_ (dref inited) (
          k {prev; curr=e} @.
          if S.slide then
            prev := e
          else
            inited := bool false
        ) (
          (prev := e) @.
          (inited := bool true)
        )
      )

  let reduce_gen : ('a exp -> 'a exp -> 'a exp) -> 
      (int exp -> e exp -> 'a exp) -> t -> ('a exp -> 'w stm) -> 'w stm =
    fun op mapper {prev;curr} ->
      op (mapper C.(int 0) curr) (mapper C.(int 1) (C.dref prev)) |> C.letl

  let reduce : (e exp -> e exp -> e exp) -> t -> 
      ('a exp -> 'w stm) -> 'w stm =
    fun op t -> reduce_gen op (fun _ x -> x) t

  let dot : 'a tbase -> ('b -> 'a exp) -> 'b array ->
      (e exp -> e exp -> e exp) -> ('a exp -> e exp -> e exp) -> 
      t -> (e exp -> 'w stm) -> 'w stm = 
    fun _typ lift weights op mul {prev;curr} ->
      assert (Array.length weights = 2);
      C.(op (mul (lift weights.(0)) curr) 
            (mul (lift weights.(1)) (dref prev))) |> C.letl
end

(* XXX Potentially, there should be something like Size2 for smaller
     sizes; just use ref cells rather than array
*)

(* ceil_power2 n is the least such m that m is an exact power of 2 and
   n <= m
*)
let ceil_power2 : int -> int = fun n ->
   let rec loop m = if m >= n then m else loop (2*m) in
   loop 1

(* The general case of sliding window *)
module Sliding(S: sig type e val etyp: e tbase
                         val size: int val slide: int end) = struct
  type e = S.e
  let etyp = S.etyp
  let size = S.size
  let hist_size = size - 1

  (* buff is the ring buffer containing the elements prior to curr: the history
     idx is the index of the slot to write the current element afterwards
     (that slot may contain the earliers element to evict, 
      if hist_size = buffer_len)
     The buffer is filled *backwards*
     We make the buffer a bit longer to make sure the best case of reduction
     (with one loop) occurs more often.
  *)
  type t = {buffer: e arr; curr: e exp; idx: int exp}
  let buffer_len = ceil_power2 (2*hist_size)
  let buff_len1  = buffer_len - 1
  let mask = C.int (buffer_len - 1)

  let make_stream : ?padding:bool -> e exp stream -> t stream = 
    fun ?(padding=false) st ->
      let- len = Raw.initializing_ref 
                      (C.int @@ if padding then hist_size else 0) in
      let- idx = Raw.initializing_ref
                      (C.int @@ 
                       if padding then (buff_len1-hist_size) land buff_len1 
                       else buff_len1) 
      in
      let- buffer = Raw.initializing_arr etyp 
                      (Array.make buffer_len C.(tbase_zero etyp)) in
      st |>
      Raw.map_raw ~linear:false C.(fun e k ->
        if_ (dref len < int hist_size) (
          array_set buffer (dref idx) e @.
          incr len @.
          (idx := logand ((dref idx) - int 1) mask)
        ) (
          k {buffer; idx = dref idx; curr=e} @.
          array_set buffer (dref idx) e @.
          (idx := logand ((dref idx) - int 1) mask) @.
          if Stdlib.(S.slide > 1) then
            len := dref len + int Stdlib.(1 - S.slide)
          else
            unit
        )
      )

  let reduce_gen : ('a exp -> 'a exp -> 'a exp) -> 
      (int exp -> e exp -> 'a exp) -> t -> ('a exp -> 'w stm) -> 'w stm =
    fun op mapper {buffer;curr;idx} k ->
      let open C in
      let- sum = newref (mapper (int 0) curr) in
      if_ (idx <= int Stdlib.(buffer_len - 1 - hist_size)) (
        (* this is a static loop and unrollable/vectorizable *)
        for_ (int 1) ~upe:(int Stdlib.(hist_size+1)) (fun i ->
          sum := op (dref sum) 
                    (mapper i (array_get' buffer (idx + i)))
        )
      ) (
        for_ (idx+int 1) ~upe:(int buffer_len) (fun i ->
          sum := op (dref sum) 
                    (mapper (i - idx) (array_get' buffer i))
        ) @.
        for_ (int 0) ~upe:(idx + int Stdlib.(hist_size-buffer_len +1)) (fun i ->
          sum := op (dref sum) 
                    (mapper (i - idx + int buffer_len)
                            (array_get' buffer i))
        )
      ) @.
      k (dref sum)
end

(* Alternative general case of a sliding window
   For the history size hist_size, the needed buffer space 
   is 2*hist_size -1. However, we allocate 2*hist_size (and waste
   one memory cell and occasional assignment to it) to avoid
   branching.
   The general idea is that writing into a buffer writes into two places
   at once, separated by hist_size. That avoids the need to copy or rotate
   the buffer. The usable window is always the sequence of consecutive
   slots.
 *)
module SlidingAlt(S: sig type e val etyp: e tbase
                         val size: int val slide: int end) = struct
  type e = S.e
  let etyp = S.etyp
  let size = S.size
  let hist_size = size - 1

  (* buff is the buffer containing the elements prior to curr: the history.
     The buffer is filled *backwards*.
     idx is the index of the most recently written element, and ranges
     0 through hist_size-1.
  *)
  type t = {buffer: e arr; curr: e exp; idx: int exp}
  let buffer_len = 2*hist_size
  let hs1  = hist_size - 1

  let make_stream : ?padding:bool -> e exp stream -> t stream = 
    fun ?(padding=false) st ->
      let- idx = Raw.initializing_ref (C.int hs1) in
      let- buffer = Raw.initializing_arr etyp 
                      (Array.make buffer_len C.(tbase_zero etyp)) in
      if padding && S.slide = 1 then (* window is always filled *)
      st |>
      Raw.map_raw C.(fun e k ->
          k {buffer; idx = dref idx; curr=e} @.
          decr idx @.
          if1 (dref idx < int 0) (idx := int hs1) @.
          array_set buffer (dref idx) e @.
          array_set buffer (dref idx + int hist_size) e 
        )
      else
      let- len = Raw.initializing_ref 
                      (C.int @@ if padding then hist_size else 0) in
      st |>
      Raw.map_raw ~linear:false C.(fun e k ->
        if_ (dref len < int hist_size) (
          decr idx @.
          if1 (dref idx < int 0) (idx := int hs1) @.
          array_set buffer (dref idx) e @.
          array_set buffer (dref idx + int hist_size) e @.
          incr len
        ) (
          k {buffer; idx = dref idx; curr=e} @.
          decr idx @.
          if1 (dref idx < int 0) (idx := int hs1) @.
          array_set buffer (dref idx) e @.
          array_set buffer (dref idx + int hist_size) e @.
          if Stdlib.(S.slide > 1) then
            len := dref len + int Stdlib.(1 - S.slide)
          else
            unit
        )
      )

  let reduce_gen : ('a exp -> 'a exp -> 'a exp) -> 
      (int exp -> e exp -> 'a exp) -> t -> ('a exp -> 'w stm) -> 'w stm =
    fun op mapper {buffer;curr;idx} k ->
      let open C in
      let- sum = newref (mapper (int 0) curr) in
      for_ (int 0) ~upe:(int hist_size) (fun i ->
          sum := op (dref sum) 
                    (mapper (i+int 1) (array_get' buffer (idx + i)))
      ) @.
      k (dref sum)
end

(* Really long and sliding-by-1 window.
     The buffer is copied from-time-to-time, so the window
     should be long enough to make it worthwhile
     (copying occurs once in window-size steps).
     The strong point: all loops have static bounds and are vectorizable.
*)
module SlidingMove(S: sig type e val etyp: e tbase 
                               val size : int end)  = struct
  type e = S.e
  let etyp = S.etyp
  let size = S.size

    (* buff is has twice the size.
       idx is the index of the slot where curr (it to be) written.
       The buffer is filled backwards! So, 
       idx+1 is the previous element, and idx+size-1 is the earliest
       element in the window.
    *)
  type t = {buffer: e arr; idx: int exp; curr: e exp;}
  let buffer_len = 2 * size

  let make_stream : ?padding:bool -> e exp stream -> t stream = 
    fun ?(padding=false) st ->
      let- idx = Raw.initializing_ref
                      (C.int @@ if padding then buffer_len - size 
                                           else buffer_len-1) in
      let- buffer = Raw.initializing_arr etyp 
                      (Array.make buffer_len C.(tbase_zero etyp))
      in
      if padding then (* The window is filled and stays full: len = size-1 *)
      st |>
      Raw.map_raw C.(fun e k ->
        if1 (dref idx < int 0) (
            (* shift elements 0 .. size-2 to bufflen+1-size .. bufflen-1 *)
                for_ (int 0) ~upe:(int Stdlib.(size-1)) (fun i ->
                  array_set buffer (i + int Stdlib.(buffer_len + 1 - size)) 
                    (array_get' buffer i)
                )
            @.
            (idx := int Stdlib.(buffer_len - size))
          ) @.
          k {buffer; idx = dref idx; curr=e} @.
          array_set buffer (dref idx) e @.
          decr idx
        )
      else
      let- len = Raw.initializing_ref 
                      (C.int @@ if padding then size-1 else 0) in
      st |>
      Raw.map_raw ~linear:false C.(fun e k ->
        if_ (dref len < int Stdlib.(size-1)) (
          array_set buffer (dref idx) e @.
          incr len @.
          decr idx
        ) (  (* len is size-1: that many useful elements in the buffer *)
          if1 (dref idx < int 0) (
            (* shift elements 0 .. size-2 to bufflen+1-size .. bufflen-1 *)
                for_ (int 0) ~upe:(int Stdlib.(size-1)) (fun i ->
                  array_set buffer (i + int Stdlib.(buffer_len + 1 - size)) 
                    (array_get' buffer i)
                )
            @.
            (idx := int Stdlib.(buffer_len - size))
          ) @.
          k {buffer; idx = dref idx; curr=e} @.
          array_set buffer (dref idx) e @.
          decr idx
        )
      )

  let reduce_gen : ('a exp -> 'a exp -> 'a exp) -> 
      (int exp -> e exp -> 'a exp) -> t -> ('a exp -> 'w stm) -> 'w stm =
    fun op mapper {buffer;idx;curr} k ->
      let open C in
      let- sum = newref (mapper (int 0) curr) in
      for_ (int 1) ~upe:(int size) (fun i ->
        sum := op (dref sum) 
                  (mapper i (array_get' buffer (idx + i)))
      ) @.
      k (dref sum)
end

 (*
  (* Really long and sliding-by-N window.
     The buffer is copied from-time-to-time, so the window
     should be long enough to make it worthwhile
     (copying occurs once in window-size steps).
     The strong point: all loops have static bounds and are vectorizable.
  *)
module SlidingNMove(S: sig type e val etyp: e tbase 
                                val size : int
                                val slide : int (* >= 1 *)
                            end)  = struct
    type e = S.e
    let etyp = S.etyp
    let size = S.size

    let _ = assert (S.slide >= 1)

    (* buff is has (slide+1) x size.
       idx is the index of the first free slot in the buffer
       So, the useful elements are
       idx-size .. idx-1
    *)
    type t = {buffer: e arr; idx: int exp}
    let buffer_len = (S.slide + 1) * size

    let make_stream : ?padding:bool -> e exp stream -> t stream = 
    fun ?(padding=false) st ->
      let- len = Raw.initializing_ref (C.int @@ if padding then size-1 else 0) in
      let- idx = Raw.initializing_ref (C.int @@ if padding then size-1 else 0) in
      let- buffer = if padding then Raw.initializing_arr etyp (Array.make buffer_len C.(tbase_zero etyp))
                               else Raw.initializing_uarr etyp buffer_len in
      st |>
      Raw.map_raw ~linear:false C.(fun e k ->
        if_ (dref len < int Stdlib.(size-1)) (
          array_set buffer (dref idx) e @.
          incr len @.
          incr idx
        ) (  (* len is size-1: how many useful elements in the buffer *)
          if1 (dref idx >= int Stdlib.(buffer_len-S.slide+1)) (
            begin 
              if Stdlib.(size-1 < memcpy_num) then
                for_ (int 0) (int Stdlib.(size-2)) (fun i ->
                  array_set buffer i 
                    (array_get' buffer (dref idx - int Stdlib.(size-1) + i))
                )
              else
                blit buffer (dref idx - int Stdlib.(size-1)) buffer (int 0) (int Stdlib.(size-1))
            end @.
            (idx := int Stdlib.(size-1))
          ) @.
          array_set buffer (dref idx) e @.
          incr idx @.
          (len := dref len - int Stdlib.(S.slide - 1)) @.
          k {buffer; idx = dref idx}
        )
      )

  let reduce_gen : ('a exp -> 'a exp -> 'a exp) -> 
      (int exp -> e exp -> 'a exp) -> t -> ('a exp -> 'w stm) -> 'w stm =
    fun op mapper {buffer;idx} k ->
      let open C in
      let- sum = newref (tbase_zero etyp) in
      let- ibeg = letl (idx - int size) in
      for_ (int 0) (int Stdlib.(size-1)) (fun i ->
        sum := op (dref sum) 
                  (mapper i (array_get' buffer (ibeg + i)))
      ) @.
      dref sum
  end

  (* Long, tumbling window
  *)
module BigTumbling(S: sig type e val etyp: e tbase 
                            val size: int end) = struct
    type e = S.e
    let etyp = S.etyp
    let size = S.size
    let hist_size = size - 1

    (* buff contains the elements prior to curr: the history
    *)
    type t = {buffer: e arr; curr: e exp}
    let buffer_len = hist_size

    let make_stream : ?padding:bool -> e exp stream -> t stream = 
    fun ?(padding=false) st ->
      let- idx    = Raw.initializing_ref (C.int @@ if padding then hist_size else 0) in
      let- buffer = if padding then Raw.initializing_arr etyp (Array.make buffer_len C.(tbase_zero etyp))
                               else Raw.initializing_uarr etyp buffer_len in
      st |>
      Raw.map_raw ~linear:false C.(fun e k ->
        if_ (dref idx < int hist_size) (
          array_set buffer (dref idx) e @.
          incr idx
        ) (
          k {buffer; curr=e} @.
          (idx := int 0)
        )
      )

  let reduce_gen : ('a exp -> 'a exp -> 'a exp) -> 
      (int exp -> e exp -> 'a exp) -> t -> ('a exp -> 'w stm) -> 'w stm =
    fun op mapper {buffer;curr} ->
      failwith "mada"
end
*)
  (* size = taps, slide = 1+decimation (Only StreamIt) . Size should be at least 2 *)
  (* the dispatcher *)
let make_window : type a. a tbase -> int -> int -> a window = 
  fun typ size slide ->
    assert (size>1);
    assert (slide>=1 && slide <= size);
    let module Parm = struct type e = a let etyp = typ let size = size end in
    if size = 2 then 
      (module Size2(struct include Parm let slide = slide=1 end))
  (*
    else if slide = size then
      (module WinEx(BigTumbling(Parm)))
  *)
    else if slide = 1 && size > 20 then
           (module WinEx(SlidingMove(Parm)))
   (*
    else if slide > 1 && size > 10 then
           (module WinEx(SlidingNMove(struct 
                    include Parm let slide=slide end)))
   *)
    else
      (module WinEx(SlidingAlt(struct include Parm let slide=slide end)))
end


