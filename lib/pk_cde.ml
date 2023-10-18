(* An implementation of the Cde signature 
   with tracking partial knowledge    

   An online partial evaluator
*)
let (>>) f g = fun x -> f x |> g

module type cde_ex     = module type of Cde_top

module Make(C: cde_ex) = struct

let ident = C.ident

(* Annotations: what is known about 'a C.cde *)
type 'a annot = 
  | Sta of 'a               (* Statically known *)
  | Global                  (* It is a exp value that does not depend
                               on the library code: it is some global
                               array or the global let-bound immutable value
                             *)
  | Unk                     (* Nothing is statically known *)

type 'a exp = {sta : 'a annot; dyn : 'a C.exp}

type 'a tbase = 'a C.tbase              (* Base types *)
let tbool  = C.tbool
let tint   = C.tint

let tbase_zero : 'a tbase -> 'a exp = fun typ ->
  {sta=Global; dyn=C.tbase_zero typ}

(* injection-projection pairs *)
let inj : 'a C.exp -> 'a exp = fun x -> {sta=Unk; dyn=x}
let dyn : 'a exp -> 'a C.exp = function {dyn=x} -> x

(* Use this for code coming `outside'; for example, for arguments
   of the generated function
 *)
let inj_global : 'a C.exp -> 'a exp = fun x -> {sta=Global; dyn=x}

(* Injection for functions *)
(* One may always use inj, inj1 or inj2: these are operations of the
   last resort
*)
let inj1 : ('a C.exp -> 'b C.exp) -> ('a exp -> 'b exp) = fun f -> function
  | {sta=Unk; dyn=x} -> inj @@ f x
  | {dyn=x}          -> {sta=Global; dyn=f x}

let inj2 : ('a C.exp -> 'b C.exp -> 'c C.exp) -> ('a exp -> 'b exp -> 'c exp) =
  fun f x y -> 
    let v = f (dyn x) (dyn y) in
    match (x,y) with
    | ({sta=Unk},_) | (_,{sta=Unk}) -> inj v
    | _ -> {sta=Global; dyn=v}

(* General lifting for functions, performing static computations where
   possible
 *)
let lift1 : 
  ('a -> 'b) -> ('b -> 'b exp) -> ('a C.exp -> 'b C.exp) -> 
  ('a exp -> 'b exp) = fun fs lift fd -> function
  | {sta=Sta x} -> fs x |> lift
  | x -> inj1 fd x

let lift2 : 
  ('a -> 'b -> 'c) -> ('c -> 'c exp) -> ('a C.exp -> 'b C.exp -> 'c C.exp) -> 
  ('a exp -> 'b exp -> 'c exp) = fun fs lift fd x y -> match (x,y) with
    | ({sta=Sta x},{sta=Sta y}) -> fs x y |> lift
    | (x,y) -> inj2 fd x y


(* Inquiries *)
let is_static : 'a exp -> bool = function {sta=Sta _} -> true | _ -> false
(* Is value dependent on something introduced by the library itself?
 *)
let is_fully_dynamic : 'a exp -> bool = function {sta=Unk} -> true | _ -> false

(* Booleans *)
let bool : bool -> bool exp = fun b -> {sta=Sta b; dyn=C.bool b}

let not : bool exp -> bool exp = function
  | {sta=Sta b} -> bool (Stdlib.not b)
      (* XXX For a global thing, do genlet? *)
  | {sta=s;dyn=y} -> {sta=s;dyn=C.not y}

let (&&) : bool exp -> bool exp -> bool exp = fun c1 c2 ->
  match (c1,c2) with
  | ({sta=Sta true},x)  | (x,{sta=Sta true})  -> x
  | ({sta=Sta false},_) | (_,{sta=Sta false}) -> bool false
  | _ -> inj2 C.(&&) c1 c2

let (||) : bool exp -> bool exp -> bool exp = fun c1 c2 ->
  match (c1,c2) with
  | ({sta=Sta true},_)  | (_,{sta=Sta true})  -> bool true
  | ({sta=Sta false},x) | (x,{sta=Sta false}) -> x
  | _ -> inj2 C.(||) c1 c2


(* Integers *)
let int : int -> int exp = fun i -> {sta=Sta i; dyn=C.int i}
let ( ~-) : int exp -> int exp            = lift1 Stdlib.( ~-) int C.( ~-) 

let ( + ) : int exp -> int exp -> int exp  = fun x y ->
  match (x,y) with
  | ({sta=Sta 0},x) | (x,{sta=Sta 0}) -> x
  | _ -> lift2 Stdlib.( + ) int C.( + ) x y
let ( - ) : int exp -> int exp -> int exp  = fun x y ->
  match (x,y) with
  | ({sta=Sta 0},y) -> ~- y
  | (x,{sta=Sta 0}) -> x
  | _ -> lift2 Stdlib.( - ) int C.( - ) x y
let ( * ) : int exp -> int exp -> int exp  = fun x y ->
  match (x,y) with
  | ({sta=Sta 0},_)  | (_,{sta=Sta 0})  -> int 0
  | ({sta=Sta 1},x)  | (x,{sta=Sta 1})  -> x
  | ({sta=Sta -1},x) | (x,{sta=Sta -1}) -> ~- x
  | _ -> lift2 Stdlib.( * ) int C.( * ) x y
let ( / ) : int exp -> int exp -> int exp  = lift2 Stdlib.( / ) int C.( / ) 
let (mod) : int exp -> int exp -> int exp  = fun x y ->
  match (x,y) with
  | (_,{sta=Sta 1}) -> int 0
  | _ -> lift2 Stdlib.(mod) int C.(mod) x y
let logand : int exp -> int exp -> int exp = lift2 Int.logand int C.logand 

let ( =)  : int exp -> int exp -> bool exp = lift2 Stdlib.( =) bool C.( =)
let ( <)  : int exp -> int exp -> bool exp = lift2 Stdlib.( <) bool C.( <)
let ( >)  : int exp -> int exp -> bool exp = lift2 Stdlib.( >) bool C.( >)
let (<=)  : int exp -> int exp -> bool exp = lift2 Stdlib.(<=) bool C.(<=)
let (>=)  : int exp -> int exp -> bool exp = lift2 Stdlib.(>=) bool C.(>=)

let imin : int exp -> int exp -> int exp = lift2 Stdlib.(min) int C.(imin) 
let imax : int exp -> int exp -> int exp = lift2 Stdlib.(max) int C.(imax) 


let map_opt : ('a -> 'b) -> 'a option -> 'b option = fun f -> function
  | None -> None
  | Some x -> Some (f x)

(* Statements *)

type 'a stm = 
  | Unit : unit stm
  | Stm : 'a C.stm -> 'a stm

let inj_stm : 'a C.stm -> 'a stm = fun x -> Stm x
let dyn_stm : type a. a stm -> a C.stm = function
  | Unit  -> C.unit
  | Stm x -> x
(* Often used for adjusting continuations *)
let injdyn : ('a exp -> 'b stm) -> 'a C.exp -> 'b C.stm = 
  fun k v -> v |> inj |> k |> dyn_stm

let ret : 'a exp -> 'a stm = fun e -> e |> dyn |> C.ret |> inj_stm

(* Local let, without movement *)
let letl : 'a exp -> (('a exp -> 'w stm) -> 'w stm) = fun x k ->
  match x with
  | {sta=Sta _} -> k x                  (* constant, no need to bind *)
  | {dyn=v}     -> inj_stm @@ C.letl v (injdyn k)  

(* possibly let-insertion *)
let glet : 'a exp -> 'a exp = function
  | {sta=Sta _} as x -> x
  | {sta=Global; dyn=x} -> {sta=Global; dyn=C.glet x}
  | {sta=Unk} -> 
      failwith "glet of Unk: let insertion with movement may be unsafe"

(* Often occuring sequential composition *)
let seq : type a. unit stm -> a stm -> a stm = fun c1 c2 ->
  match (c1,c2) with
  | (Unit, c) -> c
  | (c,Unit)  -> c
  | (Stm c1, Stm c2)     -> Stm (C.seq c1 c2)
let ( @. )  : unit stm -> 'a stm -> 'a stm = seq
let unit = Unit
let seqs    : unit stm list -> unit stm = function
  | [] -> unit
  | h::t -> List.fold_left (@.) h t



(* Reference cells *)
(* We rarely want to do any static operations on them. Actually,
   strymonas itself operates on them, so we make all references dynamic
 *)
type 'a mut = 'a C.mut

let newref  : 'a exp -> ('a mut -> 'w stm) -> 'w stm = fun x k ->
  inj_stm @@ C.newref (dyn x) (fun x -> k x |> dyn_stm)
(* could potentially guess a constant of the right type ... *)
let newuref : 'a exp -> ('a mut -> 'w stm) -> 'w stm = fun x k ->
  match x with
  | {sta=Sta _} | {sta=Global} -> newref x k
  | x -> inj_stm @@ C.newuref (dyn x) (fun x -> k x |> dyn_stm)
let dref : 'a mut -> 'a exp    = fun x -> inj @@ C.dref x
let incr : int mut -> unit stm = fun x -> inj_stm @@ C.incr x
let decr : int mut -> unit stm = fun x -> inj_stm @@ C.decr x
let (:=) : 'a mut -> 'a exp -> unit stm = fun x y -> inj_stm @@ C.(:=) x (dyn y)

(* Arrays *)
(* If we had immutable arrays, we could optimize array_get *)
type 'a arr = int exp * 'a C.arr        (* paired with the length expression *)

let array_get' : 'a arr -> int exp -> 'a exp
    = fun (_,arr) i -> inj @@ C.array_get' arr (dyn i)
(* It makes sense to combine it with letl *)
let array_get : 'a arr -> int exp -> ('a exp -> 'w stm) -> 'w stm
    = fun (_,arr) i k -> 
      inj_stm @@ C.array_get arr (dyn i) (injdyn k)
let array_len : 'a arr -> int exp = fun (len,arr) -> len 
let array_set : 'a arr -> int exp -> 'a exp -> unit stm = fun (_,arr) i v ->
  inj_stm @@ C.array_set arr (dyn i) (dyn v)
let new_uarray : 'a tbase -> int -> ('a arr -> 'w stm) -> 'w stm =
  fun tb n k -> inj_stm @@ C.new_uarray tb n (fun a -> k (int n,a) |> dyn_stm)
let new_array  : 'a tbase -> 'a exp array -> ('a arr -> 'w stm) -> 'w stm =
  fun ty arr k ->
    let len = Array.length arr |> int in
    inj_stm @@ 
    C.new_array ty (Array.map dyn arr) (fun a -> k (len,a) |> dyn_stm)
let new_static_array  : 'a tbase -> ('b -> 'a exp) -> 'b array -> 
  ('a arr -> 'w stm) -> 'w stm = fun ty cnv arr k -> 
    let len = Array.length arr |> int in
    inj_stm @@ 
    C.new_static_array ty (cnv >> dyn) arr (fun a -> k (len,a) |> dyn_stm)


(* Control operators *)
let cond : bool exp -> 'a exp -> 'a exp -> 'a exp = fun cnd bt bf ->
  match cnd with
  | {sta=Sta true}  -> bt
  | {sta=Sta false} -> bf
  | _ -> inj @@ C.cond (dyn cnd) (dyn bt) (dyn bf)

let if_  : bool exp -> unit stm -> unit stm -> unit stm = fun cnd bt bf ->
  match cnd with
  | {sta=Sta true}  -> bt
  | {sta=Sta false} -> bf
  | _ -> inj_stm @@ C.if_ (dyn cnd) (dyn_stm bt) (dyn_stm bf)

let if1  : bool exp -> unit stm -> unit stm = fun cnd bt ->
  match cnd with
  | {sta=Sta true}  -> bt
  | {sta=Sta false} -> unit
  | _ -> inj_stm @@ C.if1 (dyn cnd) (dyn_stm bt)


(* Control constructs generally create Unk code
 *)
let for_ : int exp ->           (* exact lower bound *)
           upe:int exp ->       (* least upper bound, *exclusive* *)
           ?guard:bool exp ->   (* possibly a guard, terminate when false *)
           ?step:int exp ->     (* step *)
           (int exp -> unit stm) -> unit stm 
 = fun lwb ~upe ?guard ?step body ->
  match (lwb,upe,step) with
  | ({sta=Sta i},{sta=Sta j},_) when Stdlib.(j <= i) -> unit
  | ({sta=Sta i},{sta=Sta j},None) when Stdlib.(i+1 = j) -> 
   begin match guard with
    | None -> body (int i)
    | Some g -> if1 g (body (int i))
    end
  (* XXX: possibly unroll if upb is known and small enough *)
  | _ -> inj_stm @@ C.for_ (dyn lwb) ~upe:(dyn upe) 
                  ?guard:(map_opt dyn guard) 
                  ?step:(map_opt dyn 
                           (match step with
                           | Some {sta=Sta 1} -> None
                           | x -> x))
                  (injdyn body)

let while_ : bool exp -> unit stm -> unit stm = fun goon body ->
  match goon with
  | {sta=Sta false} -> unit
  | _ -> inj_stm @@ C.while_ (dyn goon) (dyn_stm body)


(* Advanced control construction *)

(* Loop with continue: execute the body, which may request to be
   re-executed (like C `continue')
   The loop (as well as its body) have two continuations: the normal
   one is invoked with the produced value. The second one is implicit:
   it is entered when the normal continuation is NOT invoked.

   Here is how cloop is supposed to work, in the form when the second
   continuation is made explicit too:

   let cloop : ('a -> bot) ->           (* normal continuation *)
               (unit -> bot) ->         (* exceptional one *)
               (unit -> bool) ->        (* guard condition *)
               (('a -> bot) -> (unit -> bot) -> bot) ->(* body, in 2CPS style *)
               bot = fun k kexc bp body ->
   let rec loop () =                    (* create a return label *)
      if not (bp ()) then kexc ()       (* guard failed *)
      else body k loop
   in loop ()

 *)
let cloop :
    ('a -> unit stm) ->        (* cloop's continuation *)
    bool exp option ->         (* possibly a guard *)
    (('a -> unit stm) -> unit stm) ->   (* body, in CPS, which may exit
                                           without calling its continuation
                                         *)
    unit stm = fun k bp body ->
      inj_stm @@ C.cloop (fun x -> k x |> dyn_stm) (map_opt dyn bp) 
               (fun k -> body (fun x -> k x |> inj_stm) |> dyn_stm)


(* Simple i/o, useful for debugging. Newline at the end *)
let print_int   : int exp   -> unit stm = fun x -> inj_stm @@C.print_int (dyn x)

(* FFI support *)
type 'sg ff = {invoke : 'sg}

(* Numbers of various sorts *)
module type num = sig
  type t
  type num_t
  val tbase  : t tbase
  val to_t   : num_t -> t               (* for the sake of Partial Eval *)
  val of_t   : t -> num_t
  val lit    : num_t -> t exp
  val neg    : t exp -> t exp
  val ( +. ) : t exp -> t exp -> t exp
  val ( -. ) : t exp -> t exp -> t exp
  val ( *. ) : t exp -> t exp -> t exp
  val ( /. ) : t exp -> t exp -> t exp
  val equal  : t exp -> t exp -> bool exp
  val ( < )  : t exp -> t exp -> bool exp
  val ( > )  : t exp -> t exp -> bool exp
  val print  : t exp -> unit stm
end

(* Floating point numbers of various sorts *)
module type flonum = sig
  include num
  val rem  : t exp -> t exp -> t exp
  val truncate : t exp -> int exp
  val of_int   : int exp -> t exp
  val sin  : (t exp -> t exp) ff
  val cos  : (t exp -> t exp) ff
  val atan : (t exp -> t exp) ff        (* more can be added *)
end

(* Complex numbers *)
module type cmplxnum = sig
  include num
  type float_t
  val conj     : t exp -> t exp
  val norm2    : t exp -> float_t exp   (* Euclidian norm, squared *)
  val arg      : t exp -> float_t exp   (* -pi to pi *)
  val real     : t exp -> float_t exp
  val imag     : t exp -> float_t exp
  val complex  : float_t exp -> float_t exp -> t exp
  val scale    : float_t exp -> t exp -> t exp
end


module I64 : sig
  include num with type num_t = int
  val of_int   : int exp -> t exp
  end = struct
  include C.I64
  let lit    : num_t -> t exp = fun x -> {sta=Sta (to_t x); dyn=C.I64.lit x}
  let int : t -> t exp = of_t >> lit
  let tlift1 : (int -> int) -> (t -> t) = fun f -> of_t >> f >> to_t
  let tlift2 : (int -> int -> int) -> (t -> t -> t) = fun f x y -> 
    f (of_t x) (of_t y) |> to_t
  let tliftb : (int -> int -> bool) -> (t -> t -> bool) = fun f x y -> 
    f (of_t x) (of_t y)
  let neg    = lift1 (tlift1 Int.neg) int C.I64.neg 
  let ( +. ) = lift2 (tlift2 Stdlib.( + )) int C.I64.( +. ) 
  let ( -. ) = lift2 (tlift2 Stdlib.( - )) int C.I64.( -. ) 
  let ( *. ) = lift2 (tlift2 Stdlib.( * )) int C.I64.( *. ) 
  let ( /. ) = lift2 (tlift2 Stdlib.( / )) int C.I64.( /. )
  let equal  = lift2 (tliftb Stdlib.( = )) bool C.I64.( equal ) 
  let ( >  ) = lift2 (tliftb Stdlib.( > )) bool C.I64.( > )
  let ( <  ) = lift2 (tliftb Stdlib.( < )) bool C.I64.( < )
  (*
  let rem    = lift2 (tlift2 Stdlib.Int.rem) int C.I64.rem
  *)
  let of_int = lift1 (to_t) int C.I64.of_int
  let print = dyn >> C.I64.print >> inj_stm
end

module F64 : (flonum with type num_t = float and type t = C.F64.t) = struct
  include C.F64
  let lit    : num_t -> t exp = fun x -> {sta=Sta (to_t x); dyn=C.F64.lit x}
  let float : t -> t exp = of_t >> lit
  let tlift1 : (float -> float) -> (t -> t) = fun f -> of_t >> f >> to_t
  let tlift2 : (float -> float -> float) -> (t -> t -> t) = fun f x y -> 
    f (of_t x) (of_t y) |> to_t
  let tliftb : (float -> float -> bool) -> (t -> t -> bool) = fun f x y -> 
    f (of_t x) (of_t y)
  let neg    = lift1 (tlift1 Float.neg) float C.F64.neg 
  let ( +. ) = lift2 (tlift2 Stdlib.( +. )) float C.F64.( +. ) 
  let ( -. ) = lift2 (tlift2 Stdlib.( -. )) float C.F64.( -. ) 
  let ( *. ) x y = match (x,y) with
  | ({sta=Sta x},{sta=Sta y}) -> tlift2 Stdlib.( *. ) x y |> float
  | (({sta=Sta y},_)  | (_,{sta=Sta y})) when Stdlib.(of_t y = 0.)  -> lit 0.
  | (({sta=Sta y},x)  | (x,{sta=Sta y})) when Stdlib.(of_t y = 1.) -> x
  | (({sta=Sta y},x)  | (x,{sta=Sta y})) when Stdlib.(of_t y = -1.) -> neg x
  | _ -> inj2 C.F64.( *. ) x y
  let ( /. ) = lift2 (tlift2 Stdlib.( /. )) float C.F64.( /. ) 
  let rem    = lift2 (tlift2 Stdlib.Float.rem) float C.F64.rem
  let equal  = lift2 (tliftb Float.equal) bool C.F64.( equal ) 
  let ( >  ) = lift2 (tliftb Stdlib.( > )) bool C.F64.( > )
  let ( <  ) = lift2 (tliftb Stdlib.( < )) bool C.F64.( < )
  let truncate = lift1 (of_t >> Stdlib.truncate) int C.F64.truncate
  let of_int = lift1 (Stdlib.float_of_int >> to_t) float C.F64.of_int
  let print = dyn >> C.F64.print >> inj_stm
  let sin  : (t exp -> t exp) ff = {invoke = inj1 C.F64.sin.invoke}
  let cos  : (t exp -> t exp) ff = {invoke = inj1 C.F64.cos.invoke}
  let atan : (t exp -> t exp) ff = {invoke = inj1 C.F64.atan.invoke}
end

module F32 : (flonum with type num_t = float and type t = C.F32.t) = struct
  include C.F32
  let lit    : num_t -> t exp = fun x -> {sta=Sta (to_t x); dyn=C.F32.lit x}
  let float : t -> t exp = of_t >> lit
  let tlift1 : (float -> float) -> (t -> t) = fun f -> of_t >> f >> to_t
  let tlift2 : (float -> float -> float) -> (t -> t -> t) = fun f x y -> 
    f (of_t x) (of_t y) |> to_t
  let tliftb : (float -> float -> bool) -> (t -> t -> bool) = fun f x y -> 
    f (of_t x) (of_t y)
  let neg    = lift1 (tlift1 Float.neg) float C.F32.neg 
  let ( +. ) = lift2 (tlift2 Stdlib.( +. )) float C.F32.( +. ) 
  let ( -. ) = lift2 (tlift2 Stdlib.( -. )) float C.F32.( -. ) 
  let ( *. ) x y = match (x,y) with
  | ({sta=Sta x},{sta=Sta y}) -> tlift2 Stdlib.( *. ) x y |> float
  | (({sta=Sta y},_)  | (_,{sta=Sta y})) when Stdlib.(of_t y = 0.)  -> lit 0.
  | (({sta=Sta y},x)  | (x,{sta=Sta y})) when Stdlib.(of_t y = 1.) -> x
  | (({sta=Sta y},x)  | (x,{sta=Sta y})) when Stdlib.(of_t y = -1.) -> neg x
  | _ -> inj2 C.F32.( *. ) x y
  let ( /. ) = lift2 (tlift2 Stdlib.( /. )) float C.F32.( /. ) 
  let rem    = lift2 (tlift2 Stdlib.Float.rem) float C.F32.rem
  let equal  = lift2 (tliftb Float.equal) bool C.F32.( equal ) 
  let ( >  ) = lift2 (tliftb Stdlib.( > )) bool C.F32.( > )
  let ( <  ) = lift2 (tliftb Stdlib.( < )) bool C.F32.( < )
  let truncate = lift1 (of_t >> Stdlib.truncate) int C.F32.truncate
  let of_int = lift1 (Stdlib.float_of_int >> to_t) float C.F32.of_int
  let print = dyn >> C.F32.print >> inj_stm
  let sin  : (t exp -> t exp) ff = {invoke = inj1 C.F32.sin.invoke}
  let cos  : (t exp -> t exp) ff = {invoke = inj1 C.F32.cos.invoke}
  let atan : (t exp -> t exp) ff = {invoke = inj1 C.F32.atan.invoke}
end

module C32 : (cmplxnum with type num_t = Complex.t and type float_t = F32.t
      and type t = C.C32.t) = struct
  include C.C32
  let lit    : num_t -> t exp = fun x -> {sta=Sta (to_t x); dyn=C.C32.lit x}
  let cmplx : t -> t exp = of_t >> lit
  let tlift1 : (Complex.t -> Complex.t) -> (t -> t) = 
    fun f -> of_t >> f >> to_t
  let tlift2 : (Complex.t -> Complex.t -> Complex.t) -> (t -> t -> t) = 
    fun f x y -> f (of_t x) (of_t y) |> to_t
  let tliftb : (Complex.t -> Complex.t -> bool) -> (t -> t -> bool) = 
    fun f x y -> 
    f (of_t x) (of_t y)
  let neg    = lift1 (tlift1 Complex.neg) cmplx C.C32.neg 
  let ( +. ) = lift2 (tlift2 Complex.add) cmplx C.C32.( +. ) 
  let ( -. ) = lift2 (tlift2 Complex.sub) cmplx C.C32.( -. ) 
  let ( *. ) = lift2 (tlift2 Complex.mul) cmplx C.C32.( *. ) 
  let ( /. ) = lift2 (tlift2 Complex.div) cmplx C.C32.( /. ) 
  let print  = dyn >> C.C32.print >> inj_stm
  let conj   = lift1 (tlift1 Complex.conj) cmplx C.C32.conj 
  let tlift1' : (Complex.t -> float) -> (t -> float_t) = 
    fun f -> of_t >> f >> F32.to_t
  let norm2  = lift1 (tlift1' Complex.norm2) F32.(of_t >> lit) C.C32.norm2 
  let arg    = lift1 (tlift1' Complex.arg) F32.(of_t >> lit) C.C32.arg 
  let real   = lift1 (tlift1' Complex.(fun x -> x.re)) F32.(of_t >> lit) 
               C.C32.real 
  let imag   = lift1 (tlift1' Complex.(fun x -> x.im)) F32.(of_t >> lit) 
               C.C32.imag 
  let complex = lift2 
      Complex.(fun x y -> {re=F32.of_t x; im=F32.of_t y} |> to_t) 
      cmplx C.C32.complex
  let scale  = lift2 Stdlib.(fun s x -> 
    Complex.{re=(F32.of_t s) *. (of_t x).re; 
             im=(F32.of_t s) *. (of_t x).im} |> to_t) cmplx
      C.C32.scale
  let equal  = lift2 (tliftb Stdlib.( = )) bool C.C32.( equal ) 
  let ( < ) = fun x y -> F32.(norm2 x > norm2 y)
  let ( > ) = fun x y -> F32.(norm2 x < norm2 y)
end


(* Injection of arbitrary MetaOCaml code

let of_code  : 'a code -> 'a cde = fun x -> inj x
let with_cde : ('a code -> 'b cde) -> 'a cde -> 'b cde = fun k x ->
  k (dyn x)
let cde_app1 : ('a->'b) code -> 'a cde -> 'b cde = fun f x ->
  inj1 (fun x' -> .< .~f .~x' >.) x
*)


let print_code : ?name:string -> 'a stm -> unit = 
 fun ?name x -> dyn_stm x |> C.print_code ?name
let run : 'a stm -> 'a = fun x -> dyn_stm x |> C.run
let run_capture_output : unit stm -> Scanf.Scanning.in_channel = 
  dyn_stm >> C.run_capture_output

(* Top-level procedures *)

type 'a proc_t = 'a C.proc_t

let nullary_proc : 'a stm -> 'a proc_t = fun x -> 
  dyn_stm x |> C.nullary_proc 
let arg_base : ?name:string -> 'a tbase -> ('a exp -> 'b proc_t) -> 
  ('a -> 'b) proc_t = 
   fun ?name ty body -> C.arg_base ?name ty (inj_global >> body)

let arg_array : ?name:string -> ?mutble:bool -> 
  int exp ->                            (* length *)
  'a tbase -> ('a arr -> 'b proc_t) -> ('a array -> 'b) proc_t =
    fun ?name ?mutble len ty body ->
      C.arg_array ?name ?mutble (dyn len) ty (fun a -> body (len,a))

let pp_proc : ?name:string -> Format.formatter -> 'a proc_t -> unit =
  C.pp_proc

end
