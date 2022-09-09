(* abstraction for streaming, batteries, and v1 *)
module type cde = sig
  type 'a cde

  val int : int -> int cde
  val ( + ) : int cde -> int cde -> int cde
  val ( - ) : int cde -> int cde -> int cde
  val ( * ) : int cde -> int cde -> int cde
  val (mod) : int cde -> int cde -> int cde

  val ( = ) : int cde -> int cde -> bool cde
  val ( > ) : int cde -> int cde -> bool cde

  val cond : bool cde -> 'a cde -> 'a cde -> 'a cde
  val ( || ) : bool cde -> bool cde -> bool cde

  val to_code1 : ('a cde -> 'b cde) -> ('a code -> 'b code)
  val to_code2 : ('a cde * 'b cde -> 'c cde) -> ('a code * 'b code -> 'c code)
end

module CodeBasic = struct
  include Trx_code

  let to_code1 : ('a cde -> 'b cde) -> ('a code -> 'b code)
      = fun f x -> f x
  let to_code2 : ('a cde * 'b cde -> 'c cde) -> ('a code * 'b code -> 'c code)
      = fun f (x,y) -> f (x,y) 
end

module CodePV = struct
  include Pk_cde.Make(Trx_code)
  let injg : 'a Trx_code.cde -> 'a cde = fun x -> {sta=Global; dyn=x}

  let to_code1 : ('a cde -> 'b cde) -> ('a code -> 'b code)
      = fun f x -> dyn @@ f (injg x)
  let to_code2 : ('a cde * 'b cde -> 'c cde) -> ('a code * 'b code -> 'c code)
      = fun f (x,y) -> dyn @@ f (injg x, injg y) 
end


module type lib = sig
  type 'a cde
  type 'a stream

  (* Producers *)
  val of_arr : 'a array cde -> 'a stream

  (* Consumers *)
  val fold : ('z cde -> 'a cde -> 'z cde) -> 'z cde -> 'a stream -> 'z cde

  (* Transformers *)
  val map      : ('a cde -> 'b cde) -> 'a stream -> 'b stream
  val flat_map : ('a cde -> 'b stream) -> 'a stream -> 'b stream  
  val filter   : ('a cde -> bool cde) -> 'a stream -> 'a stream
  val take     : int cde -> 'a stream -> 'a stream
  val zip_with : ('a cde -> 'b cde -> 'c cde) -> ('a stream -> 'b stream -> 'c stream)


  type byte
  val byte_max: byte
  val decode : byte stream -> bool stream
end


(* ===Main Functor=== *)
module Benchmark(C : cde)(L : lib with type 'a cde = 'a C.cde) = struct
  open L
  type 'a cde = 'a C.cde
  let sum_int : int stream -> int cde = fold C.( + ) (C.int 0)

  
  let sum : int array code -> int code
  = C.to_code1 @@ fun arr ->
    of_arr arr
    |> sum_int

  let sumOfSquares : int array code -> int code
  = C.to_code1 @@ fun arr ->
    of_arr arr
    |> map C.(fun x -> x * x)
    |> sum_int

  let maps : int array code -> int code
  = C.to_code1 @@ fun arr ->
    of_arr arr
    |> map C.(fun x -> x * int 1)
    |> map C.(fun x -> x * int 2)
    |> map C.(fun x -> x * int 3)
    |> map C.(fun x -> x * int 4)
    |> map C.(fun x -> x * int 5)
    |> map C.(fun x -> x * int 6)
    |> map C.(fun x -> x * int 7)
    |> sum_int

  let filters : int array code -> int code
  = C.to_code1 @@ fun arr ->
    of_arr arr
    |> filter C.(fun x -> x > int 1)
    |> filter C.(fun x -> x > int 2)
    |> filter C.(fun x -> x > int 3)
    |> filter C.(fun x -> x > int 4)
    |> filter C.(fun x -> x > int 5)
    |> filter C.(fun x -> x > int 6)
    |> filter C.(fun x -> x > int 7)
    |> sum_int

  let sumOfSquaresEven : int array code -> int code
  = C.to_code1 @@ fun arr ->
    of_arr arr
    |> filter C.(fun x -> x mod (int 2) = int 0)
    |> map C.(fun x -> x * x)
    |> sum_int

  let cart : (int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
      of_arr arr1
      |> flat_map (fun x -> of_arr arr2 |> map C.(fun y -> x * y))
      |> sum_int

  let dotProduct : (int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    zip_with C.( * ) (of_arr arr1) (of_arr arr2)
    |> sum_int

  let flatMap_after_zipWith : (int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    zip_with C.( + ) (of_arr arr1) (of_arr arr1)
    |> flat_map (fun x -> of_arr arr2|> map C.(fun el -> el + x))
    |> sum_int


  let zipWith_after_flatMap : (int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    of_arr arr1
    |> flat_map (fun x -> of_arr arr2 |> map C.(fun y -> y + x))
    |> zip_with C.( + ) (of_arr arr1)
    |> sum_int


  let flat_map_take : (int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
        of_arr arr1
        |> flat_map (fun x -> of_arr arr2 |> map C.(fun y -> x * y))
        |> take (C.int 20_000_000)
        |> sum_int


  (* The following two benchmarks are from
   https://github.com/epfldata/staged-rewritten-streams
   but slightly modified (compexified)
   Also, zip_flat_flat code from the thesis had a mistake
  *)
  let zip_filter_filter :(int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    zip_with C.( + )
      (of_arr arr1 |> filter C.(fun x -> x > int 7))
      (of_arr arr2 |> filter C.(fun x -> x > int 5))
    |> sum_int

  (* Take more values, so that arr1 is scanned in full at least once.
   This also checks better the code, more control paths are taken.
  *)
  let zip_flat_flat :(int array code * int array code) -> int code
  = C.to_code2 @@ fun (arr1, arr2) ->
    zip_with C.( + )
      (of_arr arr1 |> 
        flat_map (fun x -> of_arr arr2 |> map C.(fun y -> x * y)))
      (of_arr arr2 |> 
        flat_map (fun x -> of_arr arr1 |> map C.(fun y -> x - y)))
    |> take (C.int 200_000_000)
    |> sum_int


  (* XXX Needed benchmarks:
     zip 
       (zip (stream |> filter) (stream |> filter))
       (zip (stream |> filter) (stream |> filter))

     And another one, with fllat_map instead of filter.

    And another one,
     zip (stream |> filter)
       (zip (stream |> filter) (stream |> filter))
   
   (perhaps, make 4 or 5 chain of zippers?)
   *)


  let decoding :(byte array code * byte array code) -> int code
  = C.to_code2 @@ fun (arr1,arr2) ->
    zip_with C.(||) (of_arr arr1 |> decode) (of_arr arr2 |> decode)
    |> map C.(fun x -> cond x (int 1) (int 0))
    |> sum_int    
end
