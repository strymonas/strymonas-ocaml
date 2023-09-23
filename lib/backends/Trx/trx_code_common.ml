(* An implementation of the Cde signature, using the 
   MetaOCaml code type and code generation facilities
*)

type 'a exp = 'a code

type 'a tbase_desc = ..    
type 'a tbase_desc +=
  | TBool  : bool tbase_desc
  | TInt   : int tbase_desc

type 'a tbase = {desc: 'a tbase_desc;  (* Base types *)
                 zero:  'a exp
                }

let tbool  : bool tbase  = {desc=TBool; zero = .<false>.}
let tint   : int tbase   = {desc=TInt; zero = .<0>.}

(* A constant of a base type: 0 or its equivalent *)
let tbase_zero : type a. a tbase -> a exp = fun {zero} -> zero

type 'a stm = 'a code

(* Local let, without movement *)
let letl : 'a exp -> (('a exp -> 'w stm) -> 'w stm) = Codelib.letl

(* possibly let-insertion *)
let glet : 'a exp -> 'a exp = genlet

(* Often occuring sequential composition *)
let seq : unit stm -> 'a stm -> 'a stm = fun c1 c2 ->
  .<.~c1; .~c2>.
let ( @. )  : unit stm -> 'a stm -> 'a stm = seq
let unit = .<()>.
let seqs    : unit stm list -> unit stm = function
  | [] -> unit
  | h::t -> List.fold_left (@.) h t

let ret  : 'a exp -> 'a stm = Fun.id


(* Booleans *)
let bool : bool -> bool exp = fun b -> .<b>.

let not : bool exp -> bool exp = fun x -> .< not .~x >.

let (&&) : bool exp -> bool exp -> bool exp = fun c1 c2 ->
  .<.~c1 && .~c2>.

let (||) : bool exp -> bool exp -> bool exp = fun c1 c2 ->
  .<.~c1 || .~c2>.

(*
let eq  : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x  = .~y >.
let neq : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x <> .~y >.
let lt  : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x  < .~y >.
let gt  : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x  > .~y >.
let leq : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x <= .~y >.
let geq : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x >= .~y >.
*)

(* Integers *)
let int : int -> int exp = fun i -> .<i>.

let ( + ) : int exp -> int exp -> int exp  = fun x y -> .< .~x + .~y >.
let ( - ) : int exp -> int exp -> int exp  = fun x y -> .< .~x - .~y >.
let ( * ) : int exp -> int exp -> int exp  = fun x y -> .< .~x * .~y >.
let ( / ) : int exp -> int exp -> int exp  = fun x y -> .< .~x / .~y >.
let (mod) : int exp -> int exp -> int exp  = fun x y -> .< .~x mod .~y >.
let ( ~-) : int exp -> int exp  = fun x -> .< ~- .~x >.

let logand  : int exp -> int exp -> int exp  = 
  fun x y -> .<Int.logand .~x .~y >.

let ( =)  : int exp -> int exp -> bool exp = fun x y -> .< .~x  = .~y >.
let ( <)  : int exp -> int exp -> bool exp = fun x y -> .< .~x  < .~y >.
let ( >)  : int exp -> int exp -> bool exp = fun x y -> .< .~x  > .~y >.
let (<=)  : int exp -> int exp -> bool exp = fun x y -> .< .~x <= .~y >.
let (>=)  : int exp -> int exp -> bool exp = fun x y -> .< .~x >= .~y >.

(* It turns out that Stdlib.min is slow! It was responsible for
   the big slowdown in one of the benchmarks. It seems OCaml does not
   inline min and generates a function call instead.
 *)
let imin : int code -> int code -> int code = fun c1 c2 ->
  Stdlib.(.<if .~c1 < .~c2 then .~c1 else .~c2>.)
let imax : int code -> int code -> int code = fun c1 c2 ->
  Stdlib.(.<if .~c1 > .~c2 then .~c1 else .~c2>.)


(* Strings *)
let string : string -> string exp = fun x -> .<x>.

(* Uninitialized value, used to create uninitialized reference cells *)
let uninit : 'a exp = .<Obj.obj (Obj.new_block 0 0)>.

(* Reference cells *)
type 'a mut = 'a ref code
let newref  : 'a exp -> ('a mut -> 'w stm) -> 'w stm = fun x k ->
  Codelib.letl ~name:"v" .< ref .~x >. k
(* could potentially guess a constant of the right type ... *)
let newuref : 'a exp -> ('a mut -> 'w stm) -> 'w stm = fun x k ->
  newref uninit k
let dref : 'a mut -> 'a exp    = fun x -> .< ! .~x >.
let incr : int mut -> unit stm = fun x -> .< incr .~x >.
let decr : int mut -> unit stm = fun x -> .< decr .~x >.
let (:=) : 'a mut -> 'a exp -> unit stm = fun x y -> .< .~x := .~y >.

let pair : 'a exp -> 'b exp -> ('a * 'b) exp = fun x y -> .<(.~x,.~y)>.
let fst : ('a * 'b) exp -> 'a exp = fun e -> .< fst .~e>.
let snd : ('a * 'b) exp -> 'b exp = fun e -> .< snd .~e>.

(* Arrays *)
(* Values of 'a arr denotes a _value_ (usually, variable name)
   Arrays are not expressions
 *)
type 'a arr = 'a array code
let array_get' : 'a arr -> int exp -> 'a exp
    = fun arr i -> .<(.~arr).(.~i)>.
(* It makes sense to combine it with letl *)
let array_get : 'a arr -> int exp -> ('a exp -> 'w stm) -> 'w stm
    = fun arr i k -> .<let el = (.~arr).(.~i) in .~(k .<el>.)>.
let array_len : 'a arr -> int exp = fun arr -> .<Array.length .~arr>.
let array_set : 'a arr -> int exp -> 'a exp -> unit stm = fun arr i v ->
  .< .~arr.(.~i) <- .~v >.

(* initialized array, immediately bound to a variable *)
let new_array  : 'a tbase -> 'a exp array -> ('a arr -> 'w stm) -> 'w stm = 
  fun _ i k -> Codelib.letl ~name:"a" (Lifts.lift_array i) k
(* elements are also statically known *)
let new_static_array  : 'a tbase -> ('b -> 'a exp) -> 'b array -> 
  ('a arr -> 'w stm) -> 'w stm = fun _ cnv arr k ->
    k (Array.map cnv arr |> Lifts.lift_array |> genlet ~name:"a")

(* new uninitialized array, of the base type
*)
let new_uarray : 'a tbase -> int -> ('a arr -> 'w stm) -> 'w stm =
  fun tb n k -> new_array tb (Array.make n (tbase_zero tb)) k

(* Control operators *)
let cond : bool exp -> 'a exp -> 'a exp -> 'a exp = fun cnd bt bf ->
  .<if .~cnd then .~bt else .~bf>.
let if_  : bool exp -> unit stm -> unit stm -> unit stm = cond
let if1  : bool exp -> unit stm -> unit stm = fun cnd bt ->
  .<if .~cnd then .~bt>.

(* In OCaml, for-loops cannot have guards, so we have to generate a
   while loop instead in this case. We also generate while-loop
   if step is specified
 *)
let for_ : int exp ->           (* exact lower bound *)
           upe:int exp ->       (* least upper bound, *exclusive* *)
           ?guard:bool exp ->   (* possibly a guard, terminate when false *)
           ?step:int exp ->     (* step *)
           (int exp -> unit stm) -> unit stm = fun lwb ~upe ?guard ?step body ->
  match (step,guard) with
  | (None,None)   -> 
              let open Stdlib in
              .<for i = .~lwb to .~upe-1 do .~(body .<i>.) done>.
  | (step,Some g) -> let open Stdlib in
              let step = Option.value step ~default:.<1>. in
              .<let i = ref .~lwb in 
                while (!i < .~ upe && .~g) do 
                  let iv = !i in i:=iv+ .~step; .~(body .<iv>.) done>.
  | (Some step,None) -> let open Stdlib in
              .<let i = ref .~lwb in 
                while (!i < .~ upe) do 
                  let iv = !i in i:=iv+ .~step; .~(body .<iv>.) done>.

let while_ : bool exp -> unit stm -> unit stm = fun goon body ->
  .<while .~goon do .~body done>.


(* Advanced control construction *)

(* Loop with continue: execute the body, which may request to be
   re-executed (like C `continue')
   This is essentially do{...} while bp loop.

   The loop (as well as its body) have two continuations: the normal
   one is invoked with the produced value. The second one is implicit:
   it is entered when the normal continuation is NOT invoked.

   At the very beginning, the guard is already checked to be true.

   Here is how cloop is supposed to work, in the form when the second
   continuation is made explicit too:

   let cloop : ('a -> bot) ->           (* normal continuation *)
               (unit -> bot) ->         (* exceptional one *)
               (unit -> bool) ->        (* guard condition *)
               (('a -> bot) -> (unit -> bot) -> bot) ->(* body, in 2CPS style *)
               bot = fun k kexc bp body ->
   let rec loop () =                    (* create a return label *)
      body k (fun () -> if bp () then loop () else kexc ())
   in loop ()

 *)
let cloop :
    ('a -> unit stm) ->        (* cloop's continuation *)
    bool exp option ->         (* possibly a guard *)
    (('a -> unit stm) -> unit stm) ->   (* body, in CPS, which may exit
                                           without calling its continuation
                                         *)
    unit stm = 
    fun k bp body ->
      newref (bool true) @@ fun again ->
        while_ (dref again) @@
          seq (body (fun x -> seq (again := bool false) (k x)))
            (match bp with
            (* It seems on modern architectures it's better to avoid
               ifs, as they disrupt the control flow and break
               pipelining
             *)
            (* | Some g -> if1 (dref again) (again := g) *)
            | Some g -> again := dref again && g
            | None   -> unit)

(* Inquiries. They are best effort *)
(* Is value statically known: known at code-generation time *)
let is_static : 'a exp -> bool = fun _ -> false
(* Is value dependent on something introduced by the library itself?
   For example, dependent on letl identifier, etc.
   It may always return true.
 *)
let is_fully_dynamic : 'a exp -> bool = fun _ -> true

(* Accumulate all elements of a finite stream in a collection *)
let nil  : 'a list exp = .<[]>.
let cons : 'a exp -> 'a list exp -> 'a list exp = fun x l -> .< .~x :: .~l >.
let rev  : 'a list exp -> 'a list exp = fun l -> .<List.rev .~l>.

(* let some  : 'a cde -> 'a option cde = fun x -> .<Some .~x>.
let none  : 'a option cde = .<None>. *)

(* Simple i/o, useful for debugging. Newline at the end *)
(* We use the Format output because it can be redirected. *)
let print_int   : int exp   -> unit stm = fun x -> 
  .<Format.print_int .~x; Format.force_newline ()>.

(* Useful for debugging
   Run the code, capture its output and then return as
   Scanf.Scanning.in_channel
   The first argument is the code running function (which differs
   for bytecode and native mode, for example)
 *)
let run_capture_output_gen : 
    (unit stm -> unit) -> unit stm -> Scanf.Scanning.in_channel = fun run c ->
  let buf = Buffer.create 128 in
  let open Format in
  let (old_out,old_flush) = get_formatter_output_functions () in
  set_formatter_output_functions (Buffer.add_substring buf) Fun.id;
  Fun.protect 
    ~finally:(fun () -> set_formatter_output_functions old_out old_flush)
    (fun () -> run c; print_flush ());
  let s = Buffer.contents buf in
  Buffer.reset buf;
  Scanf.Scanning.from_string s

let print_code : ?name:string -> 'a stm -> unit = fun ?(name="fn") x ->
    Codelib.print_code Format.std_formatter x;
    Format.fprintf Format.std_formatter "@." 

let of_code  : 'a code -> 'a exp = fun x -> x

(* foreign function type : now concrete (not private) *)
type 'sg ff =  {invoke : 'sg}

let make_ff  : 'sg -> 'sg ff = fun x -> {invoke = x}
let make_ff1 : ('a -> 'b) code -> ('a code -> 'b code) ff = fun f ->
  make_ff (fun x -> .<.~f .~x>.)

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

module F64 = struct
  type t = float
  type num_t = float
  type 'a tbase_desc +=
    | TF64   : t tbase_desc
  let to_t = Fun.id
  let of_t = Fun.id
  let tbase  : t tbase = {desc=TF64; zero = .<0.>.}
  let lit    : num_t -> t exp = fun x -> .< x >.
  let neg    : t exp -> t exp = fun x -> .<Float.neg .~x >.
  let ( +. ) : t exp -> t exp -> t exp = fun x y -> .< .~x +. .~y >.
  let ( -. ) : t exp -> t exp -> t exp = fun x y -> .< .~x -. .~y >.
  let ( *. ) : t exp -> t exp -> t exp = fun x y -> .< .~x *. .~y >.
  let ( /. ) : t exp -> t exp -> t exp = fun x y -> .< .~x /. .~y >.
  let rem    : t exp -> t exp -> t exp = fun x y -> .< Float.rem .~x .~y >.
  let truncate : t exp -> int exp = fun x -> .<Stdlib.truncate .~x>.
  let of_int   : int exp -> t exp = fun x -> .<Stdlib.Float.of_int .~x>.
  let print    : t exp -> unit stm = fun x -> .<Format.printf "%.17g@\n" .~x>.
  let sin      : (t exp -> t exp) ff = make_ff1 .<Stdlib.sin>.
  let cos      : (t exp -> t exp) ff = make_ff1 .<Stdlib.cos>.
  let atan     : (t exp -> t exp) ff = make_ff1 .<Stdlib.atan>.
end

module F32 = F64

module C32 = struct
  type t = Complex.t
  type num_t = Complex.t
  type float_t = float
  type 'a tbase_desc +=
    | TC32   : t tbase_desc
  let to_t = Fun.id
  let of_t = Fun.id
  let tbase  : t tbase = {desc=TC32; zero = .<Complex.zero>.}
      (* to avoid creating CSPs *)
  let lit    : num_t -> t exp = fun {Complex.re;im} -> .<{Complex.re=re;im=im}>.
  let neg    : t exp -> t exp = fun x -> .<Complex.neg .~x >.
  let ( +. ) : t exp -> t exp -> t exp = fun x y -> .<Complex.add .~x .~y >.
  let ( -. ) : t exp -> t exp -> t exp = fun x y -> .<Complex.sub .~x .~y >.
  let ( *. ) : t exp -> t exp -> t exp = fun x y -> .<Complex.mul .~x .~y >.
  let ( /. ) : t exp -> t exp -> t exp = fun x y -> .<Complex.div .~x .~y >.
  let print  : t exp -> unit stm = fun x -> 
    .<Format.printf "(%.17g,%.17g)@\n" (.~x).re (.~x).im>.
  let conj   : t exp -> t exp = fun x -> .<Complex.conj .~x>.
  let norm2  : t exp -> float_t exp = fun x -> .<Complex.norm2 .~x>.
  let arg    : t exp -> float_t exp = fun x -> .<Complex.arg .~x>.
  let real   : t exp -> float_t exp = fun x -> .<(.~x).re>.
  let imag   : t exp -> float_t exp = fun x -> .<(.~x).im>.
  let complex : float_t exp -> float_t exp -> t exp = fun x y -> 
    .<Complex.{re= .~x; im= .~y}>.
  let scale  : float_t exp -> t exp -> t exp = fun s x -> 
    .<Stdlib.{re= .~s *. (.~x).re; im= .~s *. (.~x).im}>.
end


(* top-level procedures *)

type 'a proc_t =
   | Proc0 : 'a stm -> 'a proc_t
   | Proc1 : ('a exp -> 'b proc_t) -> ('a -> 'b) proc_t

let nullary_proc : 'a stm -> 'a proc_t = fun x -> Proc0 x
let arg_base : ?name:string -> 'a tbase -> ('a exp -> 'b proc_t) -> 
  ('a -> 'b) proc_t = 
  fun ?(name="n") _ body -> Proc1 body
    
let arg_array : ?name:string -> ?mutble:bool -> 
  int exp ->                            (* length *)
  'a tbase -> ('a arr -> 'b proc_t) -> ('a array -> 'b) proc_t =
  fun ?(name="a") ?(mutble=false) _ _ body -> Proc1 body

let pp_proc : type a. ?name:string -> Format.formatter -> a proc_t -> unit =
  fun ?(name="fn") ppf -> function
    | Proc0 x -> Codelib.print_code Format.std_formatter (.<fun () -> .~x>.)
    | _ -> failwith "not yet"


