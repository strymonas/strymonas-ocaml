(* An implementation of the Cde signature, using the 
   MetaOCaml code type and code generation facilities
*)

type 'a cde = 'a code

type 'a tbase =                         (* Base types *)
  | TBool  : bool tbase
  | TInt   : int tbase
  | TFloat : float tbase

let tbool  : bool tbase  = TBool
let tint   : int tbase   = TInt
let tfloat : float tbase = TFloat

(* A constant of a base type: 0 or its equivalent *)
let tbase_zero : type a. a tbase -> a cde = function
  | TBool  -> .<false>.
  | TInt   -> .<0>.
  | TFloat -> .<0.>.


(* Local let, without movement *)
let letl : 'a code -> (('a code -> 'w code) -> 'w code) = Codelib.letl

(* possibly let-insertion *)
let glet : 'a cde -> 'a cde = genlet

(* Often occuring sequential composition *)
let seq : unit code -> 'a code -> 'a code = fun c1 c2 ->
  .<.~c1; .~c2>.
let ( @. )  : unit cde -> 'a cde -> 'a cde = seq

let unit = .<()>.

(* Booleans *)
let bool : bool -> bool cde = fun b -> .<b>.

let not : bool code -> bool cde = fun x -> .< not .~x >.

let (&&) : bool code -> bool code -> bool code = fun c1 c2 ->
  .<.~c1 && .~c2>.

let (||) : bool code -> bool code -> bool code = fun c1 c2 ->
  .<.~c1 || .~c2>.

let eq  : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x  = .~y >.
let neq : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x <> .~y >.
let lt  : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x  < .~y >.
let gt  : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x  > .~y >.
let leq : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x <= .~y >.
let geq : 'a cde -> 'a cde -> bool cde = fun x y -> .< .~x >= .~y >.


(* Integers *)
let int : int -> int cde = fun i -> .<i>.

let ( + ) : int cde -> int cde -> int cde  = fun x y -> .< .~x + .~y >.
let ( - ) : int cde -> int cde -> int cde  = fun x y -> .< .~x - .~y >.
let ( * ) : int cde -> int cde -> int cde  = fun x y -> .< .~x * .~y >.
let ( / ) : int cde -> int cde -> int cde  = fun x y -> .< .~x / .~y >.
let (mod) : int cde -> int cde -> int cde  = fun x y -> .< .~x mod .~y >.
let ( ~-) : int cde -> int cde  = fun x -> .< ~- .~x >.

let logand  : int cde -> int cde -> int cde  = 
  fun x y -> .<Int.logand .~x .~y >.

let ( =)  : int cde -> int cde -> bool cde = fun x y -> .< .~x  = .~y >.
let ( <)  : int cde -> int cde -> bool cde = fun x y -> .< .~x  < .~y >.
let ( >)  : int cde -> int cde -> bool cde = fun x y -> .< .~x  > .~y >.
let (<=)  : int cde -> int cde -> bool cde = fun x y -> .< .~x <= .~y >.
let (>=)  : int cde -> int cde -> bool cde = fun x y -> .< .~x >= .~y >.

(* It turns out that Stdlib.min is slow! It was responsible for
   the big slowdown in one of the benchmarks. It seems OCaml does not
   inline min and generates a function call instead.
 *)
let imin : int code -> int code -> int code = fun c1 c2 ->
  Stdlib.(.<if .~c1 < .~c2 then .~c1 else .~c2>.)
let imax : int code -> int code -> int code = fun c1 c2 ->
  Stdlib.(.<if .~c1 > .~c2 then .~c1 else .~c2>.)

(* Floating points *)
(* XXX This may output a value in inconsistent accuracy with the input. *)
let float : float -> float cde = fun x -> .<x>.

let ( +. ) : float cde -> float cde -> float cde = fun x y -> .< .~x +. .~y >.
let ( -. ) : float cde -> float cde -> float cde = fun x y -> .< .~x -. .~y >.
let ( *. ) : float cde -> float cde -> float cde = fun x y -> .< .~x *. .~y >.
let ( /. ) : float cde -> float cde -> float cde = fun x y -> .< .~x /. .~y >.
let atan   : float cde -> float cde = fun x -> .<atan .~x>.
let truncate     : float cde -> int cde = fun x -> .<truncate .~x>.
let float_of_int : int cde -> float cde = fun x -> .<float_of_int .~x>.


(* Strings *)
let string : string -> string cde = fun x -> .<x>.

(* Uninitialized value, used to create uninitialized reference cells *)
let uninit : 'a cde = .<Obj.obj (Obj.new_block 0 0)>.

(* Reference cells *)
let newref  : 'a cde -> ('a ref cde -> 'w cde) -> 'w cde = fun x k ->
  Codelib.letl ~name:"v" .< ref .~x >. k
(* could potentially guess a constant of the right type ... *)
let newuref : 'a cde -> ('a ref cde -> 'w cde) -> 'w cde = fun x k ->
  newref uninit k
let dref : 'a ref cde -> 'a cde    = fun x -> .< ! .~x >.
let incr : int ref cde -> unit cde = fun x -> .< incr .~x >.
let decr : int ref cde -> unit cde = fun x -> .< decr .~x >.
let (:=) : 'a ref cde -> 'a cde -> unit cde = fun x y -> .< .~x := .~y >.

let pair : 'a cde -> 'b cde -> ('a * 'b) cde = fun x y -> .<(.~x,.~y)>.
let fst : ('a * 'b) cde -> 'a cde = fun e -> .< fst .~e>.
let snd : ('a * 'b) cde -> 'b cde = fun e -> .< snd .~e>.

(* XXX same as the tbase_zero *)
let init_value : type a. a tbase -> a cde = function
  | TBool  -> bool false
  | TInt   -> int 0
  | TFloat -> float 0.

(* Arrays *)
(* Values of 'a array cde denotes a _value_ (usually, variable name),
   never an expression to create such an array
 *)
let array_get' : 'a array cde -> int cde -> 'a cde
    = fun arr i -> .<(.~arr).(.~i)>.
(* It makes sense to combine it with letl *)
let array_get : 'a array cde -> int cde -> ('a cde -> 'w cde) -> 'w cde
    = fun arr i k -> .<let el = (.~arr).(.~i) in .~(k .<el>.)>.
let array_len : 'a array cde -> int cde = fun arr -> .<Array.length .~arr>.
let array_set : 'a array cde -> int cde -> 'a cde -> unit cde = fun arr i v ->
  .< .~arr.(.~i) <- .~v >.

(* initialized array, immediately bound to a variable *)
let new_array  : 'a tbase -> 'a cde array -> ('a array cde -> 'w cde) -> 
  'w cde = 
  fun _ i k -> Codelib.letl ~name:"a" (Lifts.lift_array i) k

(* new uninitialized array, of the base type
*)
let new_uarray : 'a tbase -> int -> ('a array cde -> 'w cde) -> 'w cde =
  fun tb n k -> new_array tb (Array.make n (init_value tb)) k

(* Control operators *)
(* Perhaps separate if_ expression from if-statement *)
let cond : bool cde -> 'a cde -> 'a cde -> 'a cde = fun cnd bt bf ->
  .<if .~cnd then .~bt else .~bf>.
let if_  : bool cde -> unit cde -> unit cde -> unit cde = cond
let if1  : bool cde -> unit cde -> unit cde = fun cnd bt ->
  .<if .~cnd then .~bt>.

(* In OCaml, for-loops cannot have guards, so we have to generate a
   while loop instead in this case. We also generate while-loop
   if step is specified
 *)
let for_ : int cde ->           (* exact lower bound *)
           int cde ->           (* exact upper bound *)
           ?guard:bool cde ->   (* possibly a guard, terminate when false *)
           ?step:int cde ->     (* step *)
           (int cde -> unit cde) -> unit cde = fun lwb upb ?guard ?step body ->
  match (step,guard) with
  | (None,None)   -> .<for i = .~lwb to .~upb do .~(body .<i>.) done>.
  | (step,Some g) -> let open Stdlib in
              let step = Option.value step ~default:.<1>. in
              .<let i = ref .~lwb in 
                while (!i <= .~ upb && .~g) do 
                  let iv = !i in i:=iv+ .~step; .~(body .<iv>.) done>.
  | (Some step,None) -> let open Stdlib in
              .<let i = ref .~lwb in 
                while (!i <= .~ upb) do 
                  let iv = !i in i:=iv+ .~step; .~(body .<iv>.) done>.

let while_ : bool cde -> unit cde -> unit cde = fun goon body ->
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
    ('a -> unit cde) ->        (* cloop's continuation *)
    bool cde option ->         (* possibly a guard *)
    (('a -> unit cde) -> unit cde) ->   (* body, in CPS, which may exit
                                           without calling its continuation
                                         *)
    unit cde = 
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
let is_static : 'a cde -> bool = fun _ -> false
(* Is value dependent on something introduced by the library itself?
   For example, dependent on letl identifier, etc.
   It may always return true.
 *)
let is_fully_dynamic : 'a cde -> bool = fun _ -> true

(* Accumulate all elements of a finite stream in a collection *)
let nil  : 'a list cde = .<[]>.
let cons : 'a cde -> 'a list cde -> 'a list cde = fun x l -> .< .~x :: .~l >.
let rev  : 'a list cde -> 'a list cde = fun l -> .<List.rev .~l>.

(* let some  : 'a cde -> 'a option cde = fun x -> .<Some .~x>.
let none  : 'a option cde = .<None>. *)

(* Simple i/o, useful for debugging. Newline at the end *)
(* We use the Format output because it can be redirected. *)
let print_int   : int cde   -> unit cde = fun x -> 
  .<Format.print_int .~x; Format.force_newline ()>.
let print_float : float cde -> unit cde = fun x -> 
  .<Format.printf "%g@\n" .~x>.

(* Useful for debugging
   Run the code, capture its output and then return as
   Scanf.Scanning.in_channel
   The first argument is the code running function (which differs
   for bytecode and native mode, for example)
 *)
let run_capture_output : 
    (unit cde -> unit) -> unit cde -> Scanf.Scanning.in_channel = fun run c ->
  let buf = Buffer.create 128 in
  let open Format in
  let (old_out,old_flush) = get_formatter_output_functions () in
  set_formatter_output_functions (Buffer.add_substring buf) Fun.id;
  Fun.protect 
    ~finally:(fun () ->set_formatter_output_functions old_out old_flush)
    (fun () -> run c; print_flush ());
  let s = Buffer.contents buf in
  Buffer.reset buf;
  Scanf.Scanning.from_string s


(* Injection of arbitrary MetaOCaml code
 *)
let of_code  : 'a code -> 'a cde = fun x -> x
let with_cde : ('a code -> 'b cde) -> 'a cde -> 'b cde = (@@)
let cde_app1 : ('a -> 'b) code -> 'a cde -> 'b cde = fun f x ->
  (fun x' -> .< .~f .~x' >.) x

let time : unit cde -> float cde = fun unit -> .<Sys.time ()>.
