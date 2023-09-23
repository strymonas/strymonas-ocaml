(* An implementation of the Cde signature, using Offshoring IR *)

(*
#load "offshoring.cma";;
*)

let ident = "C"

module I = OffshoringIR

let (>>) f g = fun x -> f x |> g

(* Base type representation *)
type 'a tbase_desc = ..
type 'a tbase_desc +=
  | TBbool  : bool tbase_desc
  | TBint   : int tbase_desc

type 'a tbase = {desc: 'a tbase_desc;
                 ityp: I.typ;          (* IR type *)
                 zero: I.exp;          (* The zero literal of that type;
                                           often used as initializer *)
                 printf_fn: I.exp -> I.block;
                 deser: string -> 'a   (* Used in unit tests *)
                }

(* type representations, and the correspondence between OCaml and IR types
*)
let make_printf : string -> I.exp list -> I.block = fun fmt es ->
   I.(of_exp @@ FunCall (I.OP.name "printf", (Const (Const_string fmt) :: es)))

let tbool  : bool  tbase =
   {desc=TBbool; ityp=I.TBool; zero=I.(Const (Const_bool false));
    printf_fn = I.(fun e -> make_printf "%s" 
                             [Cond(e,[Const(Const_string "true")],
                                     [Const(Const_string "false")])]);
    deser=((=)"true")}

(* The default int type. Our benchmarks use int accumulators that do need
   64-bits
*)
let tint_ntyp = I.I64
let tint   : int   tbase =
   {desc=TBint; ityp=I.TNum tint_ntyp; 
    printf_fn = (fun e -> make_printf "%ld" [e]);
    zero=I.(Const (Const_num (tint_ntyp,"0")));
    deser=int_of_string;}


(* Expressions *)
type 'a exp = 'a tbase * I.exp             (* Expressions, only of base types *)

let exp : 'a tbase -> I.exp -> 'a exp =
  fun typ exp -> (typ,exp)

let exp_app : 'a tbase -> I.OP.t -> I.exp list -> 'a exp =
  fun typ op args -> exp typ @@ I.(FunCall(op,args))

(* The initializer for the given type *)
let tbase_init : type a. a tbase -> a exp = fun ({zero;_} as typ) ->
   exp typ zero

let tbase_zero = tbase_init


let bool : bool -> bool exp = fun x -> exp tbool I.(Const (Const_bool x))

let unary_op : type a b. a tbase * I.OP.t * b tbase -> a exp -> b exp =
  fun (_,op,tout) (_,exp) ->
    exp_app tout op [exp]

let binary_op : type a b c. a tbase * b tbase * I.OP.t * c tbase -> 
  a exp -> b exp -> c exp =
  fun (_,_,op,tout) (_,e1) (_,e2) ->
    exp_app tout op [e1;e2]

let not  : bool exp -> bool exp = unary_op (tbool,I.OP.NOT,tbool)

let (&&) : bool exp -> bool exp -> bool exp = fun (ty,e1) (_,e2) -> 
  exp ty @@ I.And (e1,[e2])
let (||) : bool exp -> bool exp -> bool exp = fun (ty,e1) (_,e2) -> 
  exp ty @@ I.Or (e1,[e2])

let int : int -> int exp = fun x -> 
  exp tint I.(Const (Const_num (tint_ntyp, string_of_int x)))
let ( ~-)  = unary_op (tint,I.OP.NEG tint_ntyp,tint)

let ( + )  = binary_op (tint,tint,I.OP.ADD tint_ntyp,tint)
let ( - )  = binary_op (tint,tint,I.OP.SUB tint_ntyp,tint)
let ( * )  = binary_op (tint,tint,I.OP.MUL tint_ntyp,tint)
let ( / )  = binary_op (tint,tint,I.OP.DIV tint_ntyp,tint)
let (mod)  = binary_op (tint,tint,I.OP.MOD tint_ntyp,tint)
let logand = binary_op (tint,tint,I.OP.BAND tint_ntyp,tint)
let logor  = binary_op (tint,tint,I.OP.BOR tint_ntyp,tint)

let ( =) = binary_op (tint,tint,I.OP.EQ tint_ntyp,tbool)
let ( <) = binary_op (tint,tint,I.OP.LT tint_ntyp,tbool)
let ( >) = binary_op (tint,tint,I.OP.GT tint_ntyp,tbool)
let (>=) = binary_op (tint,tint,I.OP.GE tint_ntyp,tbool)
let (<=) = binary_op (tint,tint,I.OP.LE tint_ntyp,tbool)

let cond : bool exp -> 'a exp -> 'a exp -> 'a exp =
  fun (_,ce) (_,te) (ty,ee) ->
    exp ty @@ I.Cond(ce,[te],[ee])
(* XXX Add min/max to offshoringIR *)
let imin : int exp -> int exp -> int exp = fun x y -> cond (x < y) x y
let imax : int exp -> int exp -> int exp = fun x y -> cond (x < y) y x


(* Statements *)
(* Type of statements *)
type 'a tys = 
  | TVoid : unit tys
  | TBase : 'a tbase -> 'a tys

type 'a stm = 'a tys * I.block                      (* Statements *)

let unit : unit stm = (TVoid,I.Unit)

let stmt : I.stmt -> unit stm = fun s -> (TVoid, I.(Block (Sq.empty,s)))

let stmt_app : I.OP.t -> I.exp list -> unit stm =
  fun op args -> (TVoid, I.(of_exp (FunCall(op,args))))

let seq  : unit stm -> 'a stm -> 'a stm = fun (_,s1) (ty,s2) ->
  (ty,I.seq s1 s2)

let ( @. )  : unit stm -> 'a stm -> 'a stm = seq
let seqs    : unit stm list -> unit stm = function
  | [] -> unit
  | h::t -> List.fold_left (@.) h t

let ret  : 'a exp -> 'a stm = fun (ty,e) -> (TBase ty, I.of_exp e)

(* Simple i/o, useful for debugging. Newline at the end *)
let print_int : int exp -> unit stm = fun (ty,e) ->
   (TVoid, make_printf "%ld\n" [e])


(* Local let, without movement
   Permissible only in statement contexts
*)
let letl : 'a exp -> (('a exp -> 'w stm) -> 'w stm) = 
  fun (ty,exp) k ->
    match exp with
    (* Too simple (atomic) to let-bind *)
    | Const _ | LocalVar _ | GlobalVar _ -> k (ty,exp)
    | _ -> 
        let open I in
        let id = genvarname "t" in
        let binding = (Some {id;ty=ty.ityp;mut=Cnst;attrs=[]},exp) in
        let (bt,b) = k (ty,LocalVar id) in
        match b with 
        | Unit -> (bt, of_exp exp)
        | Block (bs,stm) ->
            (bt, Block (Sq.(one binding @ bs), stm))

let glet : 'a exp -> 'a exp = Fun.id    (* possibly let-insertion, at some
                                           higher place. That is, movement
                                           is OK
                                         *)

let if_  : bool exp -> unit stm -> unit stm -> unit stm =
  fun (_,ce) (_,st) (_,se) -> stmt @@ I.If(ce,st,Some se)

let if1  : bool exp -> unit stm -> unit stm =
  fun (_,ce) (_,st) -> stmt @@ I.If(ce,st,None)

let while_ : bool exp -> unit stm -> unit stm = fun (_,ce) (_,body) ->
    stmt @@ I.While([ce],body)

let for_ : int exp ->           (* exact lower bound *)
           upe:int exp ->       (* least upper bound, *exclusive* *)
           ?guard:bool exp ->   (* possibly a guard, terminate when false *)
           ?step:int exp ->     (* step *)
           (int exp -> unit stm) -> unit stm =
  fun (_,lwb) ~upe ?guard ?(step=int 1) body ->
  let open I in
  let id = genvarname "i" in
  let (_, b) = body (tint,LocalVar id) in
  stmt I.(For {id; ty=TNum tint_ntyp; lwb; upe=snd upe; step=snd step; 
               guard=Option.map snd guard; body=b})



(* Mutable Variables *)
type 'a mut = 'a tbase * I.varname    (* The name of the mutable variable *)

let dref : 'a mut -> 'a exp = fun (ty,v) -> 
   exp_app ty (I.OP.DEREF ty.ityp) [I.MutVar v]

let (:=) :  'a mut -> 'a exp -> unit stm = fun (ty,v) (_,e) -> 
  stmt_app (I.OP.ASSIGN ty.ityp) [MutVar v;e]

let incr    : int mut -> unit stm = fun (ty,v) -> 
  stmt_app (I.OP.INCR tint_ntyp) [MutVar v]

let decr    : int mut -> unit stm = fun (ty,v) -> 
  stmt_app (I.OP.DECR tint_ntyp) [MutVar v]

let newref  : 'a exp -> ('a mut -> 'w stm) -> 'w stm = fun (ty,e) body ->
   let id = I.genvarname "x" in
   let (tb,b) = body (ty,id) in
   (tb,
     (* If the body is empty, there is no need to bind anything.
        Still, the initializer must be executed.
     *)
   match b with
     | I.Unit -> I.of_exp e
     | I.Block (bindings,b) ->
         I.(Block (Sq.(one (Some {id;ty=ty.ityp;mut=Mut;attrs=[]},e) @
                   bindings), 
                   b))
    )


(* Create an *uninitialized* reference cell. The first argument is
   not an initial value: it is just a hint to get the type.
   Therefore, the first argument may be unusable as the value.
   A backend may still use some other value of the same type as
   the initial value.
   Actually, according to C standard, uninitialized numeric variables
   are actually initialized with zero.
 *)
let newuref : 'a exp -> ('a mut -> 'w stm) -> 'w stm = fun (ty,_) body ->
   let id = I.genvarname "x" in
   let (tb,b) = body (ty,id) in
   (tb,
   match b with
     | I.Unit -> I.Unit
     | I.Block (bindings,b) ->
         I.(Block (Sq.(one (Some {id;ty=ty.ityp;mut=Mut;attrs=[]},ty.zero) @
                   bindings), 
                   b))
    )


(* Arrays *)

type 'a arr = 'a tbase * int exp * (* The length exp, immut variable or const *)
               I.varname           (* The name of the array variable *)

let array_get' : 'a arr -> int exp -> 'a exp =
  fun (ty,_,a) (_,i) -> 
    exp_app ty (I.OP.Array1_get ty.ityp) [LocalVar a;i]

let array_len  : 'a arr -> int exp = fun (_,e,a) -> e

let array_set : 'a arr -> int exp -> 'a exp -> unit stm =
  fun (_,_,a) (_,i) (ty,v) ->
    stmt_app (I.OP.Array1_set ty.ityp) [LocalVar a; i; v]

(* Could be defined as a macro in C *)
let array_incr : 'a arr -> int exp -> 'a exp -> unit stm =
  fun (_,_,a) (_,i) (_,v) ->
    stmt_app (I.OP.name "array1_incr") [LocalVar a; i; v]

let array_get  : 'a arr -> int exp -> ('a exp -> 'w stm) -> 'w stm =
  fun arr i k -> letl (array_get' arr i) k

(* initialized non-empty array, immediately bound to a variable.
   Must be of a base type
 *)
let new_array  : 'a tbase -> 'a exp array -> ('a arr -> 'w stm) -> 'w stm =
   fun ty arrs body -> 
   let n = Array.length arrs in
   let len = int n in
   let id = I.genvarname "a" in
   let (tb,b) = body (ty,len,id) in
   let binding = I.(Some {id;ty=TLenArray1 (n,ty.ityp);mut=Cnst;attrs=[]},
                    Array (Array.to_list arrs |> List.map snd))
   in
   (tb,
   match b with
     | I.Unit -> I.Unit
     | I.Block (bindings,b) ->
         I.(Block (Sq.(one binding @ bindings), b))
    )

(* The same but the elements are statically known, and the array
   should be made static
*)
let new_static_array  : 'a tbase -> ('b -> 'a exp) -> 'b array -> 
  ('a arr -> 'w stm) -> 'w stm =
   fun ty cnv arrs body -> 
   let n = Array.length arrs in
   let len = int n in
   let id = I.genvarname "a" in
   let (tb,b) = body (ty,len,id) in
   let binding = I.(Some {id;ty=TLenArray1 (n,ty.ityp);mut=Cnst;
                          attrs=[A_static]},
                    Array (Array.map (cnv >> snd) arrs |> Array.to_list))
   in
   (tb,
   match b with
     | I.Unit -> I.Unit
     | I.Block (bindings,b) ->
         I.(Block (Sq.(one binding @ bindings), b))
    )


(* new uninitialized array, of the base type
*)
let new_uarray : 'a tbase -> int -> ('a arr -> 'w stm) -> 'w stm = 
   fun ty n body -> 
   let len = int n in
   let id = I.genvarname "a" in
   let (tb,b) = body (ty,len,id) in
   let binding = I.(Some {id;ty=TLenArray1 (n,ty.ityp);mut=Cnst;attrs=[]},
                    Array [])
   in
   (tb,
   match b with
     | I.Unit -> I.Unit
     | I.Block (bindings,b) ->
         I.(Block (Sq.(one binding @ bindings), b))
    )

(*
let new_uarray : 'a tbase -> int -> ('a array cde -> 'w cde) -> 'w cde =
  fun t n k -> 
    let aname = genname "a" in
    let def = ((aname,{specs=[];typ=Tarray (n,cbtype t)}),Init_none) in
    let r = k (of_desc @@ Varr(t,aname,Const(Const_num (string_of_int n)))) in
    {r with vars = Seq.push_first def r.vars}
*)

(* Inquiries. They are best effort *)
(* Is value statically known: known at code-generation time *)
let is_static : 'a exp -> bool = function
  | (_,I.Const _) -> true
  | _             -> false
(* Is value dependent on something introduced by the library itself?
   For example, dependent on letl identifier, etc.
   It may always return true.
 *)
let is_fully_dynamic : 'a exp -> bool = function
  | (_,I.Const _)     -> false
  | (_,I.GlobalVar _) -> false
  | _                 -> true


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
(* If we are going to use GOTO, make sure that body uses the continuation
   argument tail-recursively!
 *)
(*
let cloop :
    ('a -> unit cde) ->        (* cloop's continuation *)
    bool cde option ->         (* possibly a guard *)
    (('a -> unit cde) -> unit cde) ->   (* body, in CPS, which may exit
                                           without calling its continuation
                                         *)
    unit cde = 
    fun k bp body ->
      let lab = genname "l" in
      let goto = {vars=Seq.empty; 
                  inits=Seq.push (GOTO lab) Seq.empty;
                  desc=Stm} in
      let {vars;inits;desc=Exp(_,bp)} = 
        Option.value bp ~default:(bool true) in
      let inits =
        Seq.push (LABEL (lab,NOP)) @@
        Seq.push (DOWHILE (bp,
                           to_block @@ body (fun x -> seq (k x) goto))) @@
        inits in
        {vars;inits;desc=Stm}
*)
let cloop :
    ('a -> unit stm) ->        (* cloop's continuation *)
    bool exp option ->         (* possibly a guard. 
                                  It is true at the beginning *)
    (('a -> unit stm) -> unit stm) ->   (* body, in CPS, which may exit
                                           without calling its continuation,
                                           in which case the loop is re-executed
                                           provided the guard is still true.
                                         *)
   unit stm = 
   fun k bp body ->
      newref (bool true) @@ fun again ->
      let test = 
        match bp with
        | None -> dref again
        | Some bp -> dref again && bp
      in
   (* XXX Use DO WHILE? *)
      while_ test @@ 
      body (fun x -> seq (k x) (again := bool false))

(* foreign function type : now concrete (not private) *)
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

  let tbase : t tbase =
   {desc=TF64; ityp=I.TNum F64; 
    zero=I.(Const (Const_num (F64,"0.")));
    printf_fn = (fun e -> make_printf "%.17g\n" [e]);
    deser=float_of_string;}

  let print    : t exp -> unit stm = fun (_,e) -> (TVoid, tbase.printf_fn e)

  (* see longer comments in trx.ml: float_constant *)
  let lit : num_t -> t exp = fun x -> 
    let s = 
      if Float.is_integer x then string_of_float x else
      Printf.sprintf "%.17g" x
    in
    exp tbase I.(Const (Const_num (F64, s)))

  let neg   = unary_op (tbase,I.OP.NEG F64,tbase)

  let ( +.) = binary_op (tbase,tbase,I.OP.ADD F64,tbase)
  let ( -.) = binary_op (tbase,tbase,I.OP.SUB F64,tbase)
  let ( *.) = binary_op (tbase,tbase,I.OP.MUL F64,tbase)
  let ( /.) = binary_op (tbase,tbase,I.OP.DIV F64,tbase)
  let rem   = binary_op (tbase,tbase,I.OP.MOD F64,tbase)

  let truncate : t exp -> int exp = fun (_,e) -> 
    exp_app tint I.(OP.CAST {from=F64; onto=tint_ntyp}) [e]

  let of_int   : int exp -> t exp = fun (_,e) -> 
    exp_app tbase I.(OP.CAST {onto=F64; from=tint_ntyp}) [e]


  let sin : (t exp -> t exp) ff = 
    {invoke = unary_op (tbase,I.OP.name "sin",tbase)}
  let cos : (t exp -> t exp) ff = 
    {invoke = unary_op (tbase,I.OP.name "cos",tbase)}
  let atan : (t exp -> t exp) ff = 
    {invoke = unary_op (tbase,I.OP.name "atan",tbase)}
end

module F32 = struct
  type t = float
  type num_t = float
  type 'a tbase_desc +=
    | TF32   : t tbase_desc
  let to_t = Fun.id
  let of_t = Fun.id

  let tbase : t tbase =
   {desc=TF32; ityp=I.TNum F32; 
    zero=I.(Const (Const_num (F32,"0.")));
    printf_fn = (fun e -> make_printf "%.8g\n" [e]);
    deser=float_of_string;}

  (* see longer comments in trx.ml: float_constant *)
  let lit : num_t -> t exp = fun x -> 
    let s = 
      if Float.is_integer x then string_of_float x else
      Printf.sprintf "%.8g" x
    in
    exp tbase I.(Const (Const_num (F32, s)))

  let neg   = unary_op (tbase,I.OP.NEG F32,tbase)

  let ( +.) = binary_op (tbase,tbase,I.OP.ADD F32,tbase)
  let ( -.) = binary_op (tbase,tbase,I.OP.SUB F32,tbase)
  let ( *.) = binary_op (tbase,tbase,I.OP.MUL F32,tbase)
  let ( /.) = binary_op (tbase,tbase,I.OP.DIV F32,tbase)
  let rem   = binary_op (tbase,tbase,I.OP.MOD F32,tbase)

  let truncate : t exp -> int exp = fun (_,e) -> 
    exp_app tint I.(OP.CAST {from=F32; onto=tint_ntyp}) [e]

  let of_int   : int exp -> t exp = fun (_,e) -> 
    exp_app tbase I.(OP.CAST {onto=F32; from=tint_ntyp}) [e]

  let print    : t exp -> unit stm = fun (_,e) -> (TVoid, tbase.printf_fn e)

  let sin : (t exp -> t exp) ff = 
    {invoke = unary_op (tbase,I.OP.name "sinf",tbase)}
  let cos : (t exp -> t exp) ff = 
    {invoke = unary_op (tbase,I.OP.name "cosf",tbase)}
  let atan : (t exp -> t exp) ff = 
    {invoke = unary_op (tbase,I.OP.name "atanf",tbase)}
end

(* https://en.cppreference.com/w/c/numeric/complex *)

module C32 = struct
  type t = Complex.t
  type num_t = Complex.t
  type float_t = F32.t
  type 'a tbase_desc +=
    | TC32   : t tbase_desc
  let to_t = Fun.id
  let of_t = Fun.id

  let tbase : t tbase =
   {desc=TC32; ityp=I.TNum C32; 
    zero=I.(Const (Const_num (C32,"CMPLXF(0,0)")));
    printf_fn = I.(fun e -> make_printf "%.1f%+.1f*I\n" [
          FunCall (I.OP.name "crealf", [e]);
          FunCall (I.OP.name "cimagf", [e]);]);
    deser=fun s -> 
      Scanf.sscanf s "%f%c%f*I"
            Stdlib.(fun x s y -> 
               Complex.{re=x;im=if s='+' then y else Float.neg y});}

  (* see longer comments in trx.ml: float_constant *)
  let lit : num_t -> t exp = fun x -> 
    let s x = 
      if Float.is_integer x then string_of_float x else
      Printf.sprintf "%.8g" x
    in
    exp tbase I.(Const (Const_num 
                 (C32, Printf.sprintf "CMPLXF(%s,%s)" (s x.re) (s x.im))))

  let neg   = unary_op (tbase,I.OP.NEG C32,tbase)

  let ( +.) = binary_op (tbase,tbase,I.OP.ADD C32,tbase)
  let ( -.) = binary_op (tbase,tbase,I.OP.SUB C32,tbase)
  let ( *.) = binary_op (tbase,tbase,I.OP.MUL C32,tbase)
  let ( /.) = binary_op (tbase,tbase,I.OP.DIV C32,tbase)

  let print    : t exp -> unit stm = fun (_,e) -> (TVoid, tbase.printf_fn e)

  let conj     : t exp -> t exp =  unary_op (tbase,I.OP.name "conjf",tbase)
  let arg      : t exp -> float_t exp = 
    unary_op (tbase,I.OP.name "cargf",F32.tbase)
  let real     : t exp -> float_t exp =
    unary_op (tbase,I.OP.name "crealf",F32.tbase)
  let imag     : t exp -> float_t exp =
    unary_op (tbase,I.OP.name "cimagf",F32.tbase)
  let norm2    : t exp -> float_t exp = fun e -> real (e *. conj e)
  let complex  : float_t exp -> float_t exp -> t exp = fun (_,x) (_,y) ->
    exp tbase I.(FunCall (I.OP.name "CMPLXF",[x;y]))
  let scale    : float_t exp -> t exp -> t exp = fun (_,s) (_,e) ->
    exp tbase I.(FunCall (I.OP.MUL C32, [s;e]))
end


(* Procedures *)
type 'a proc_t = I.proc_t

let nullary_proc : type a. a stm -> a proc_t = fun (ty,b) -> 
  let ityp = match ty with
   | TVoid -> I.TVoid
   | TBase ty -> ty.ityp 
  in
  ([],ityp,b)

let arg_base ?(name="n") : 
    'a tbase -> ('a exp -> 'b proc_t) -> ('a -> 'b) proc_t =
  fun ty body ->
    let n = I.genvarname name in
    let (args,ct,block) = body (ty,I.LocalVar n) in
    ((n,ty.ityp)::args,ct,block)


let arg_array ?(name="a") : ?mutble:bool -> 
  int exp ->                            (* length *)
  'a tbase -> ('a arr -> 'b proc_t) -> ('a -> 'b) proc_t =
  fun ?(mutble=false) len ty body ->
    let a = I.genvarname name in
    let (args,ct,block) = body (ty,len,a) in
    ((a,I.TArray1 ty.ityp)::args,ct,block)


(* Writing the code *)

let pp_prelude : Format.formatter -> unit = fun ppf -> 
  Format.pp_print_string ppf
    "#include <stdio.h>\n#include <stdlib.h>\n
     #include <math.h>\n#include <stdbool.h>\n
     #include <complex.h>\n"

(*
   main has the return type of int, always.
   The body of main does not have to have return statement.
*)
let pp_main : Format.formatter -> unit stm -> unit = fun ppf s ->
  let (_,b) = seq s (ret (int 0)) in
  OffshoringIR_pp.pp_to_c ~out:ppf ~name:"main" ([],I.TNum I32,b)

let pp_proc : ?name:string -> Format.formatter -> 'a proc_t -> unit =
  fun ?(name="fn") ppf proc -> 
  OffshoringIR_pp.pp_to_c ~out:ppf ~name proc

(* For debugging only *)
let print_code ?name x =
  nullary_proc x |> pp_proc ?name Format.std_formatter;
  Format.fprintf Format.std_formatter "@." 

(* Debugging support: running the code *)

(* Run a block and capture its standard output, which is
   returned as an input channel (which needs to be closed).
   Typically that channel is open on a temp file, which is automatically
   removed when the channel is closed.
   We use Scanf.Scanning.in_channel for uniformity with Trx module:
   such channels can be opened on strings, buffers, etc.
 *)

let build_capture_output : 
  (Format.formatter -> unit) -> Scanf.Scanning.in_channel = fun builder ->
  let (cdename,cdechan) = Filename.open_temp_file ~temp_dir:"/tmp" "cde" ".c" in
  let ppf = Format.formatter_of_out_channel cdechan in
  builder ppf;
  Format.pp_print_flush ppf ();
  close_out cdechan;
  let outname = Filename.temp_file ~temp_dir:"/tmp" "out" ".txt" in
  let rc = Printf.kprintf Sys.command 
      "cd /tmp && gcc -Wall -lm %s && ./a.out > %s" cdename outname in
  if Stdlib.(rc = 0) then begin
    let r = Scanf.Scanning.open_in outname in
    Sys.remove cdename;
    Sys.remove outname;
    r
  end
  else failwith "compilation errors; examine the files"

let run_capture_output : unit stm -> Scanf.Scanning.in_channel = fun s ->
  build_capture_output (fun ppf -> pp_prelude ppf; pp_main ppf s)
 
(* Only for base types *)
let run : type a. a stm -> a = function 
  | (TBase ty,b) -> 
      let cin = build_capture_output (fun ppf ->
        pp_prelude ppf; 
        OffshoringIR_pp.pp_to_c ~out:ppf ~name:"fn" ([],ty.ityp,b);
        pp_main ppf (TVoid, ty.printf_fn (FunCall (I.OP.name "fn",[]))))
      in
      let r = Scanf.bscanf cin "%s@\n" ty.deser in 
      Scanf.Scanning.close_in cin; r
  | _ -> failwith "run: only base types"

;;
