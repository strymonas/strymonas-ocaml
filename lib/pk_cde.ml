(* An implementation of the Cde signature 
   with tracking partial knowledge    
*)

module type cde_ex     = module type of Cde_ex

module Make(C: cde_ex) = struct

(* Annotations: what is known about 'a C.cde *)
type 'a annot = 
  | Sta of 'a               (* Statically known *)
  | Global                  (* It is a cde value that does not depend
                               on the library code: it is some global
                               array or the global let-bound immutable value
                             *)
  | Unk                     (* Nothing is statically known *)

type 'a cde = {sta : 'a annot; dyn : 'a C.cde}

type 'a tbase = 'a C.tbase              (* Base types *)
let tbool  = C.tbool
let tint   = C.tint
let tfloat = C.tfloat

let tbase_zero : 'a tbase -> 'a cde = fun typ ->
  {sta=Global; dyn=C.tbase_zero typ}

(* injection-projection pairs *)
let inj : 'a C.cde -> 'a cde = fun x -> {sta=Unk; dyn=x}
let dyn : 'a cde -> 'a C.cde = function {dyn=x} -> x
(* Often used for adjusting continuations *)
let injdyn : ('a cde -> 'b cde) -> 'a C.cde -> 'b C.cde = 
  fun k v -> v |> inj |> k |> dyn

(* Use this for code coming `outside'; for example, for arguments
   of the generated function
 *)
let inj_global : 'a C.cde -> 'a cde = fun x -> {sta=Global; dyn=x}

(* Injection for functions *)
(* One may always use inj, inj1 or inj2: these are operations of the
   last resort
*)
let inj1 : ('a C.cde -> 'b C.cde) -> ('a cde -> 'b cde) = fun f -> function
  | {sta=Unk; dyn=x} -> inj @@ f x
  | {dyn=x}          -> {sta=Global; dyn=f x}

let inj2 : ('a C.cde -> 'b C.cde -> 'c C.cde) -> ('a cde -> 'b cde -> 'c cde) =
  fun f x y -> 
    let v = f (dyn x) (dyn y) in
    match (x,y) with
    | ({sta=Unk},_) | (_,{sta=Unk}) -> inj v
    | _ -> {sta=Global; dyn=v}

(* General lifting for functions, performing static computations where
   possible
 *)
let lift1 : 
  ('a -> 'b) -> ('b -> 'b cde) -> ('a C.cde -> 'b C.cde) -> 
  ('a cde -> 'b cde) = fun fs lift fd -> function
  | {sta=Sta x} -> fs x |> lift
  | x -> inj1 fd x

let lift2 : 
  ('a -> 'b -> 'c) -> ('c -> 'c cde) -> ('a C.cde -> 'b C.cde -> 'c C.cde) -> 
  ('a cde -> 'b cde -> 'c cde) = fun fs lift fd x y -> match (x,y) with
    | ({sta=Sta x},{sta=Sta y}) -> fs x y |> lift
    | (x,y) -> inj2 fd x y


(* Inquiries *)
let is_static : 'a cde -> bool = function {sta=Sta _} -> true | _ -> false
(* Is value dependent on something introduced by the library itself?
 *)
let is_fully_dynamic : 'a cde -> bool = function {sta=Unk} -> true | _ -> false


let map_opt : ('a -> 'b) -> 'a option -> 'b option = fun f -> function
  | None -> None
  | Some x -> Some (f x)

(* Local let, without movement *)
let letl : 'a cde -> (('a cde -> 'w cde) -> 'w cde) = fun x k ->
  match x with
  | {sta=Sta _} -> k x                  (* constant, no need to bind *)
  | {dyn=v}     -> inj @@ C.letl v (injdyn k)  

(* possibly let-insertion *)
let glet : 'a cde -> 'a cde = function
  | {sta=Sta _} as x -> x
  | {sta=Global; dyn=x} -> {sta=Global; dyn=C.glet x}
  | {sta=Unk} -> 
      failwith "glet of Unk: let insertion with movement may be unsafe"

(* Often occuring sequential composition *)
let seq : unit cde -> 'a cde -> 'a cde = fun c1 c2 ->
  match (c1,c2) with
  | ({sta=Sta ()}, _) -> c2
  | _ -> inj2 C.seq c1 c2
let ( @. )  : unit cde -> 'a cde -> 'a cde = seq

let unit = {sta=Sta (); dyn=C.unit}

(* Booleans *)
let bool : bool -> bool cde = fun b -> {sta=Sta b; dyn=C.bool b}

let not : bool cde -> bool cde = function
  | {sta=Sta b} -> bool (Stdlib.not b)
      (* XXX For a global thing, do genlet? *)
  | {sta=s;dyn=y} -> {sta=s;dyn=C.not y}

let (&&) : bool cde -> bool cde -> bool cde = fun c1 c2 ->
  match (c1,c2) with
  | ({sta=Sta true},x)  | (x,{sta=Sta true})  -> x
  | ({sta=Sta false},_) | (_,{sta=Sta false}) -> bool false
  | _ -> inj2 C.(&&) c1 c2

let (||) : bool cde -> bool cde -> bool cde = fun c1 c2 ->
  match (c1,c2) with
  | ({sta=Sta true},_)  | (_,{sta=Sta true})  -> bool true
  | ({sta=Sta false},x) | (x,{sta=Sta false}) -> x
  | _ -> inj2 C.(||) c1 c2

(*
let eq  : 'a cde -> 'a cde -> bool cde = fun x y -> lift2 Stdlib.( =) bool C.(eq) x y
let neq : 'a cde -> 'a cde -> bool cde = fun x y -> lift2 Stdlib.(<>) bool C.(neq) x y
let lt  : 'a cde -> 'a cde -> bool cde = fun x y -> lift2 Stdlib.( <) bool C.(lt) x y
let gt  : 'a cde -> 'a cde -> bool cde = fun x y -> lift2 Stdlib.( >) bool C.(gt) x y
let leq : 'a cde -> 'a cde -> bool cde = fun x y -> lift2 Stdlib.(<=) bool C.(leq) x y
let geq : 'a cde -> 'a cde -> bool cde = fun x y -> lift2 Stdlib.(>=) bool C.(geq) x y
*)



(* Integers *)
let int : int -> int cde = fun i -> {sta=Sta i; dyn=C.int i}
let ( ~-) : int cde -> int cde            = lift1 Stdlib.( ~-) int C.( ~-) 

let ( + ) : int cde -> int cde -> int cde  = fun x y ->
  match (x,y) with
  | ({sta=Sta 0},x) | (x,{sta=Sta 0}) -> x
  | _ -> lift2 Stdlib.( + ) int C.( + ) x y
let ( - ) : int cde -> int cde -> int cde  = fun x y ->
  match (x,y) with
  | ({sta=Sta 0},y) -> ~- y
  | (x,{sta=Sta 0}) -> x
  | _ -> lift2 Stdlib.( - ) int C.( - ) x y
let ( * ) : int cde -> int cde -> int cde  = fun x y ->
  match (x,y) with
  | ({sta=Sta 0},_) | (_,{sta=Sta 0}) -> int 0
  | ({sta=Sta 1},x) | (x,{sta=Sta 1}) -> x
  | _ -> lift2 Stdlib.( * ) int C.( * ) x y
let ( / ) : int cde -> int cde -> int cde  = lift2 Stdlib.( / ) int C.( / ) 
let (mod) : int cde -> int cde -> int cde  = fun x y ->
  match (x,y) with
  | (_,{sta=Sta 1}) -> int 0
  | _ -> lift2 Stdlib.(mod) int C.(mod) x y
let logand : int cde -> int cde -> int cde = lift2 Int.logand int C.logand 

let ( =)  : int cde -> int cde -> bool cde = fun x y -> lift2 Stdlib.( =) bool C.( =) x y
let ( <)  : int cde -> int cde -> bool cde = fun x y -> lift2 Stdlib.( <) bool C.( <) x y
let ( >)  : int cde -> int cde -> bool cde = fun x y -> lift2 Stdlib.( >) bool C.( >) x y
let (<=)  : int cde -> int cde -> bool cde = fun x y -> lift2 Stdlib.(<=) bool C.(<=) x y
let (>=)  : int cde -> int cde -> bool cde = fun x y -> lift2 Stdlib.(>=) bool C.(>=) x y

let imin : int cde -> int cde -> int cde = lift2 Stdlib.(min) int C.(imin) 
let imax : int cde -> int cde -> int cde = lift2 Stdlib.(max) int C.(imax) 

(* Floating points *)
let float : float -> float cde = fun x -> {sta=Sta x; dyn=C.float x}

let ( +. ) : float cde -> float cde -> float cde = 
  lift2 Stdlib.( +. ) float C.( +. ) 
let ( -. ) : float cde -> float cde -> float cde = 
  lift2 Stdlib.( -. ) float C.( -. ) 
let ( *. ) : float cde -> float cde -> float cde = 
  lift2 Stdlib.( *. ) float C.( *. ) 
let ( /. ) : float cde -> float cde -> float cde = 
  lift2 Stdlib.( /. ) float C.( /. ) 
let atan : float cde -> float cde = lift1 Stdlib.atan float C.atan
let truncate : float cde -> int cde = lift1 Stdlib.truncate int C.truncate
let float_of_int : int cde -> float cde = 
  lift1 Stdlib.float_of_int float C.float_of_int


(*
(* Strings *)
let string : string -> string cde = fun x ->
  C.{sta=Sta x; dyn = .<x>.}
*)

(* Reference cells *)
(* We rarely want to do any static operations on them. Actually,
   strymonas itself operates on them, so we make all references dynamic
 *)
let newref  : 'a cde -> ('a ref cde -> 'w cde) -> 'w cde = fun x k ->
  inj @@ C.newref (dyn x) (injdyn k)
(* could potentially guess a constant of the right type ... *)
let newuref : 'a cde -> ('a ref cde -> 'w cde) -> 'w cde = fun x k ->
  match x with
  | {sta=Sta _} | {sta=Global} -> newref x k
  | x -> inj @@ C.newuref (dyn x) (injdyn k)
let dref : 'a ref cde -> 'a cde    = fun x -> inj @@ C.dref (dyn x)
let incr : int ref cde -> unit cde = fun x -> inj @@ C.incr (dyn x)
let decr : int ref cde -> unit cde = fun x -> inj @@ C.decr (dyn x)
let (:=) : 'a ref cde -> 'a cde -> unit cde = fun x y -> 
  inj @@ C.(:=) (dyn x) (dyn y)

(* Arrays *)
let array_get' : 'a array cde -> int cde -> 'a cde
    = fun arr i -> inj @@ C.array_get' (dyn arr) (dyn i)
(* It makes sense to combine it with letl *)
let array_get : 'a array cde -> int cde -> ('a cde -> 'w cde) -> 'w cde
    = fun arr i k -> 
      inj @@ C.array_get (dyn arr) (dyn i) (injdyn k)
let array_len : 'a array cde -> int cde = fun arr -> 
  lift1 Array.length int C.array_len arr
let array_set : 'a array cde -> int cde -> 'a cde -> unit cde = fun arr i v ->
  inj @@ C.array_set (dyn arr) (dyn i) (dyn v)
let new_uarray : 'a tbase -> int -> ('a array cde -> 'w cde) -> 'w cde =
  fun tb n k -> inj @@ C.new_uarray tb n (injdyn k)
let new_array  : 'a tbase -> 'a cde array -> ('a array cde -> 'w cde) -> 
  'w cde =
  fun tb arr k ->
    inj @@ C.new_array tb (Array.map dyn arr) (fun darr ->
      let a =
        if Array.for_all (function {sta=Sta _} -> true | _ -> false) arr then
          {sta = 
           Sta (Array.map (function {sta=Sta x} -> x | _ -> assert false) arr);
           dyn = darr}
        else if Array.exists (function {sta=Unk} -> true | _ -> false) arr then
          inj darr
        else
          {sta=Global; dyn=darr}
      in k a |> dyn)

(* Control operators *)
let cond : bool cde -> 'a cde -> 'a cde -> 'a cde = fun cnd bt bf ->
  match cnd with
  | {sta=Sta true}  -> bt
  | {sta=Sta false} -> bf
  | _ -> inj @@ C.cond (dyn cnd) (dyn bt) (dyn bf)

let if_  : bool cde -> unit cde -> unit cde -> unit cde = cond
let if1  : bool cde -> unit cde -> unit cde = fun cnd bt ->
  match cnd with
  | {sta=Sta true}  -> bt
  | {sta=Sta false} -> unit
  | _ -> inj @@ C.if1 (dyn cnd) (dyn bt)


(* Control constructs generally create Unk code
 *)
let for_ : int cde ->           (* exact lower bound *)
           int cde ->           (* exact upper bound *)
           ?guard:bool cde ->   (* possibly a guard, terminate when false *)
           ?step:int cde ->     (* step *)
           (int cde -> unit cde) -> unit cde 
 = fun lwb upb ?guard ?step body ->
  match (lwb,upb) with
  | ({sta=Sta i},{sta=Sta j}) when Stdlib.(j < i) -> unit
  | ({sta=Sta i},{sta=Sta j}) when Stdlib.(i = j) -> begin match guard with
    | None -> body (int i)
    | Some g -> if1 g (body (int i))
    end
  (* XXX: possibly unroll if upb is known and small enough *)
  | _ -> inj @@ C.for_ (dyn lwb) (dyn upb) 
                  ?guard:(map_opt dyn guard) 
                  ?step:(map_opt dyn 
                           (match step with
                           | Some {sta=Sta 1} -> None
                           | x -> x))
                  (injdyn body)

let while_ : bool cde -> unit cde -> unit cde = fun goon body ->
  inj @@ C.while_ (dyn goon) (dyn body)


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
    ('a -> unit cde) ->        (* cloop's continuation *)
    bool cde option ->         (* possibly a guard *)
    (('a -> unit cde) -> unit cde) ->   (* body, in CPS, which may exit
                                           without calling its continuation
                                         *)
    unit cde = fun k bp body ->
      inj @@ C.cloop (fun x -> k x |> dyn) (map_opt dyn bp) 
               (fun k -> body (fun x -> k x |> inj) |> dyn)

(* Accumulate all elements of a finite stream in a collection 
let nil : 'a list cde = {sta=Sta []; dyn=C.nil}
let cons : 'a cde -> 'a list cde -> 'a list cde = fun x y -> inj2 C.cons x y
let rev : 'a list cde -> 'a list cde = fun l -> inj1 C.rev l;;
*)

(* Simple i/o, useful for debugging. Newline at the end *)
let print_int   : int cde   -> unit cde = inj1 C.print_int
let print_float : float cde -> unit cde = inj1 C.print_float

(* Injection of arbitrary MetaOCaml code

let of_code  : 'a code -> 'a cde = fun x -> inj x
let with_cde : ('a code -> 'b cde) -> 'a cde -> 'b cde = fun k x ->
  k (dyn x)
let cde_app1 : ('a->'b) code -> 'a cde -> 'b cde = fun f x ->
  inj1 (fun x' -> .< .~f .~x' >.) x

let time : unit cde -> float cde = fun x -> inj @@ C.time (dyn x)

*)

end
