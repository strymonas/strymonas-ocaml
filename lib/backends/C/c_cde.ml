(* An implementation of the Cde signature, using C AST
 * XXX Future work: add to var reference an indicator if a variable
   is mutable (it is if created by newref or passed as such as an
   argument). Then we can tell if an expression depends on a mutable
   variable, and generate const modifiers and add initializers to
   variable declarations more often.
   XXX: a better idea: use C99, which allows to intersperse declarations
   and expressions.
*)

open C_ast

module Seq : sig
  type 'a t
  val empty    : 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val push_first : 'a -> 'a t -> 'a t
  (* Eliminating exact duplicates *)
  val merge : 'a t -> 'a t -> 'a t
  val to_list : 'a t -> 'a list
 end = struct
   type 'a t = 'a list                 (* in REVERSE order *)
   let empty = []
   let is_empty x = x = []
   let push x l = x :: l
   let push_first x l = l @ [x]
   let merge l1 l2 = List.(filter (fun x -> not (memq x l1)) l2 @ l1) 
   let to_list = List.rev
end

type 'a tbase_desc = ..
type 'a tbase_desc +=
  | TBbool  : bool tbase_desc
  | TBint   : int tbase_desc
  | TBfloat : float tbase_desc

type 'a tbase = {desc: 'a tbase_desc;
                 ctyp: typ;             (* The C type *)
                 zero:  expression;     (* The zero literal of that type;
                                           often used as initializer *)
                 printf_fmt: string;    (* format for printf *)
                                        (* Adjuster for printf: cast, etc. *)
                 printf_cast: expression->expression;
                 deser: string -> 'a    (* For OCaml type *)
                }

(* type representations, and the correspondence between OCaml and C types
*)
let tbool  : bool  tbase = 
   {desc=TBbool; ctyp=Tbool; zero=(Const (Const_num "false"));
    printf_fmt="%s";
    printf_cast=(fun e -> Cond(e,Const(Const_string "true"),
                                 Const(Const_string "false")));
    deser=((=)"true")}

let tint   : int   tbase = 
   {desc=TBint; ctyp=Tint64; zero=(Const (Const_num "0"));
    printf_fmt="%ld";                   (* XXX Actually should use %lld and
                                           long long *)
    printf_cast=(fun e -> Cast({typ=Tlong;specs=[]},e));
    deser=int_of_string;}

let tfloat : float tbase = 
   {desc=TBfloat; ctyp=Tdouble; zero=(Const (Const_num "0"));
    printf_fmt="%g";
    printf_cast=Fun.id;
    deser=float_of_string;}

type 'a cde = 
    {vars: definition Seq.t;            (* local bindings *)
     inits: statement Seq.t;            (* statements, including initializers
                                           of the bindings *)
     desc: 'a desc}
 and 'a desc =
  | Stm  : unit desc                     (* statement *)
    (* ordinary expression. Its type is never void, that is, not unit! *)
    (* If we have local bindings, we also need statements that
       initialize these bindings.
       vars is the sequence of local binding.
       So, consider this as a sequence of statements followed by an expression.
       For example:
          int x; int i;
          x = 0;
          for(i=0; i<10; i++) x += i;
          x + 10
     *)
  | Exp  : 'a tbase * expression -> 'a desc  (* only of base types, for now *)
  | Vref : 'a tbase * varname    -> 'a ref desc  (* mutable variable *)
    (* An array is represented by a variable denoting array
       (of the type tbase []) plus an integer expression for the array
       size. That expression is either a variable or a constant.
     *)
  | Varr : 'a tbase * varname * expression -> 'a array desc

let cbtype : type a. ?specs:spec list -> a tbase -> ctype =
  fun ?(specs=[]) {ctyp;_} -> {specs;typ=ctyp}

let of_desc : 'a desc -> 'a cde = fun desc -> 
  {vars=Seq.empty; inits=Seq.empty; desc}

let exp : expression -> 'a tbase -> 'a cde =
  fun exp typ -> of_desc (Exp(typ,exp))


(* The initializer for the given type *)
let tbase_init : type a. a tbase -> a cde = fun ({zero;_} as typ) ->
   exp zero typ

let tbase_zero = tbase_init


(* Utilities *)
let assign : expression -> expression -> statement = fun el er ->
  BIMOD(ASSIGN,el,er)

let assign_var : varname -> expression -> statement = fun v er ->
  assign (Var v) er


(* Utility for binary operation *)
let merge : 'a cde -> 'b cde ->
  (definition Seq.t -> statement Seq.t -> 'a desc -> 'b desc -> 'c cde) ->
  'c cde = 
  fun {vars=vars1;inits=i1;desc=desc1} 
      {vars=vars2;inits=i2;desc=desc2} 
      k ->
   k (Seq.merge vars1 vars2) (Seq.merge i1 i2) desc1 desc2

(* Utility for ternary operation *)
let merge3 : 'a cde -> 'b cde -> 'c cde ->
  (definition Seq.t -> statement Seq.t -> 
    'a desc -> 'b desc -> 'c desc -> 'd cde) -> 'd cde = 
  fun {vars=vars1;inits=i1;desc=desc1} 
      {vars=vars2;inits=i2;desc=desc2} 
      {vars=vars3;inits=i3;desc=desc3} 
      k ->
   k (Seq.merge vars1 (Seq.merge vars2 vars3)) 
     (Seq.merge i1 (Seq.merge i2 i3)) desc1 desc2 desc3

let merge4 : 'a cde -> 'b cde -> 'c cde -> 'd cde ->
  (definition Seq.t -> statement Seq.t -> 
    'a desc -> 'b desc -> 'c desc -> 'd desc -> 'e cde) -> 'e cde = 
  fun {vars=vars1;inits=i1;desc=desc1} 
      {vars=vars2;inits=i2;desc=desc2} 
      {vars=vars3;inits=i3;desc=desc3} 
      {vars=vars4;inits=i4;desc=desc4} 
      k ->
   k (Seq.merge vars1 (Seq.merge vars2 (Seq.merge vars3 vars4))) 
     (Seq.merge i1 (Seq.merge i2 (Seq.merge i3 i4))) 
     desc1 desc2 desc3 desc4


(* Often occurring sequential composition *)
let seq : type a. unit cde -> a cde -> a cde = fun c1 c2 ->
  merge c1 c2 @@ fun vars inits d1 d2 ->
  match d1 with
  | Stm -> {vars;inits;desc=d2}
  | Exp _  -> assert false
let ( @. )  : unit cde -> 'a cde -> 'a cde = seq

let unit : unit cde = of_desc Stm

(* possibly let-insertion 
   Continuing the earlier example: if we had as an expression
          int x; int i;
          x = 0;
          for(i=0; i<10; i++) x += i;
          x + 10
  then after glet it becomes
          int x; int i; int y;
          x = 0;
          for(i=0; i<10; i++) x += i;
          y = x + 10;
          y
*)

let genname : string -> string =
  let counter = ref 0 in
  fun nm -> incr counter; nm ^ "_" ^ string_of_int !counter

let glet : type a. a cde -> a cde = fun x -> x

(* Local let, without movement *)
let letl : type a w. a cde -> ((a cde -> w cde) -> w cde) = fun e k ->
 match e with
   | {desc=Stm} -> 
       failwith "let for expression of unit type: doesn't make sense"
   | {desc=Vref _} as e -> k e
   | {desc=Varr _} as e -> k e
 (* If var is mutable, we definitely want to create the binding!
    For mutable v, Var v actually means variable dereference
   | {desc=Exp (_,Var _)}
  *)
   | {desc=Exp (_,Const _)} as e -> k e
         (* Perhaps consider expressions like x+1 to be simple and
            not warranting let-insertion: if we can show that x is immutable
            (which is easy to show: x is single-assignment unless it is
            created by a newref)
          *)
   | {vars;inits;desc=Exp (typ,e)} ->
       let v = genname "t" in
       let def = ((v,cbtype typ),Init_none) in
       let vars  = Seq.push def vars in
       let inits = Seq.push (assign_var v e) inits in
       seq {vars;inits;desc=Stm} @@ k (exp (Var v) typ)

(* Reference cells *)
let newref  : 'a cde -> ('a ref cde -> 'w cde) -> 'w cde = function 
  | {vars;inits;desc=Exp(typ,(Const _ as exp))} ->
    fun k ->
      let v = genname "v" in
      let def = ((v,cbtype typ),Init_single exp) in
      let vars = Seq.push def vars in
      seq {vars;inits;desc=Stm} @@ k (of_desc (Vref (typ,v)))
  | {vars;inits;desc=Exp(typ,exp)} ->
    fun k ->
      let v = genname "v" in
      let def = ((v,cbtype typ),Init_none) in
      let vars  = Seq.push def vars in
      let inits = Seq.push (assign_var v exp) inits in
      seq {vars;inits;desc=Stm} @@ k (of_desc (Vref (typ,v)))
  | _ -> failwith "newref are permitted on base types only"

(* Uninitialized cell *)
let newuref : type a w. a cde -> (a ref cde -> w cde) -> w cde = function
  | {desc=Exp(typ,_)} ->
      fun k -> 
        let v = genname "v" in
        let def = ((v,cbtype typ),Init_none) in
        let r = k (of_desc (Vref (typ,v))) in
        {r with vars = Seq.push_first def r.vars}
  | {desc=Varr (t,a,e)} -> 
      failwith ("newuref are permitted on base types only: varr:" ^ a) 
  | _ -> failwith "newuref are permitted on base types only"

let dref : 'a ref cde -> 'a cde = function 
   | {vars;inits;desc=Vref(typ,v)} -> {vars;inits;desc=Exp(typ,Var v)}
   | _ -> assert false

let incr : int ref cde -> unit cde =  function
   | {vars;inits;desc=Vref(typ,v)} ->
      let inits = Seq.push (UNMOD(POSINCR,Var v)) inits in
      {vars;inits;desc=Stm}
   | _ -> assert false

let decr : int ref cde -> unit cde = function
   | {vars;inits;desc=Vref(typ,v)} ->
     let inits = Seq.push (UNMOD(POSDECR,Var v)) inits in
     {vars;inits;desc=Stm}
   | _ -> assert false

let (:=) : 'a ref cde -> 'a cde -> unit cde = fun x y ->
  merge x y @@ fun[@warning "-8"] vars inits (Vref(typ,v)) desc ->
    match desc with
    | Exp (_,exp) ->
        let inits = Seq.push (assign_var v exp) inits in
        {vars;inits;desc=Stm}
    | _ -> assert false

(* Expressions *)

(* Make sure all side-effecting operations are combined into the expression
   itself, so the initializers are empty. The bindings, if present, should
   only contain constant initializers.
   Only proper expressions can be used in control constructs such as
   conditional statements, conditions in while-loop, etc. cases.
   Such expressions can be safely executed zero or more than one time.
 *)
let proper_exp : type a. a cde -> a cde = function
  | ({inits} as e) when Seq.is_empty inits -> e
  | {vars;inits;desc=Exp(t,exp)} ->
      let desc = Exp(t,Comma (Seq.to_list inits,exp)) in
      let inits = Seq.empty in
      {vars;inits;desc}
  | _ -> assert false

let unary_op : type a b. a tbase * unary_operator * b tbase -> a cde -> b cde =
  fun (_,op,t) e -> match e with
  | {desc=Exp(_,exp)} -> {e with desc=Exp(t,Unary(op,exp))}
  | _ -> assert false

let binary_op : type a b. a tbase * binary_operator * b tbase -> 
  a cde -> a cde -> b cde = fun (_,op,t) x y ->
  merge x y @@ fun vars inits d1 d2 ->
    match (d1,d2) with
    | (Exp (_,e1), Exp (_,e2)) -> 
        {vars;inits;desc=Exp(t,Binary(op,e1,e2))}
    | _ -> assert false
 
(* Booleans *)
let bool : bool -> bool cde = fun b -> 
  (* XXX Should it be TRUE and FALSE *)
  exp (Const (Const_num (if b then "1" else "0"))) tbool

let not : bool cde -> bool cde = unary_op (tbool,NOT,tbool)

let (&&) : bool cde -> bool cde -> bool cde = fun x y ->
  binary_op (tbool,AND,tbool) x (proper_exp y)
let (||) : bool cde -> bool cde -> bool cde = fun x y ->
  binary_op (tbool,OR,tbool) x (proper_exp y)


(* Integers *)
let int : int -> int cde = fun x ->
  exp (Const (Const_num (string_of_int x))) tint

let ( + ) : int cde -> int cde -> int cde  = binary_op (tint,ADD,tint)
let ( - ) : int cde -> int cde -> int cde  = binary_op (tint,SUB,tint)
let ( * ) : int cde -> int cde -> int cde  = binary_op (tint,MUL,tint)
let ( / ) : int cde -> int cde -> int cde  = binary_op (tint,DIV,tint)
let (mod) : int cde -> int cde -> int cde  = binary_op (tint,MOD,tint)
let ( ~-) : int cde -> int cde  = unary_op (tint,MINUS,tint)
let logand : int cde -> int cde -> int cde = 
  binary_op (tint,BAND,tint)

let ( =)  : int cde -> int cde -> bool cde = binary_op (tint,EQ,tbool)
let ( <)  : int cde -> int cde -> bool cde = binary_op (tint,LT,tbool)
let ( >)  : int cde -> int cde -> bool cde = binary_op (tint,GT,tbool)
let (<=)  : int cde -> int cde -> bool cde = binary_op (tint,LE,tbool)
let (>=)  : int cde -> int cde -> bool cde = binary_op (tint,GE,tbool)

let imin : int cde -> int cde -> int cde = fun c1 c2 ->
  merge c1 c2 @@ fun vars inits (Exp(_,e1)) (Exp (_,e2)) ->
    {vars;inits;desc=Exp(tint,Cond(Binary(LT,e1,e2),e1,e2))}
let imax : int cde -> int cde -> int cde = fun c1 c2 ->
  merge c1 c2 @@ fun vars inits (Exp(_,e1)) (Exp (_,e2)) ->
    {vars;inits;desc=Exp(tint,Cond(Binary(GT,e1,e2),e1,e2))}

(* Floating points *)
(* We create hexadecimal literals to avoid loss of precision due
   to conversions
*)
let float : float -> float cde = fun x ->
  let str = if Float.is_integer x then string_of_float x else
            Printf.sprintf "%h" x
  in
  exp (Const (Const_num str)) tfloat

let ( +.) : float cde -> float cde -> float cde  = binary_op (tfloat,ADD,tfloat)
let ( -.) : float cde -> float cde -> float cde  = binary_op (tfloat,SUB,tfloat)
let ( *.) : float cde -> float cde -> float cde  = binary_op (tfloat,MUL,tfloat)
let ( /.) : float cde -> float cde -> float cde  = binary_op (tfloat,DIV,tfloat)

let atan : float cde -> float cde = 
  function {desc=Exp(_,exp)} as e -> 
    {e with desc=Exp(tfloat,Call (Var "atan",[exp]))}

let truncate : float cde -> int cde = 
  function {desc=Exp(_,exp)} as e -> 
    {e with desc=Exp(tint,Cast(cbtype tint,exp))}

let float_of_int : int cde -> float cde =
  function {desc=Exp(_,exp)} as e -> 
    {e with desc=Exp(tfloat,Cast(cbtype tfloat,exp))}

(* Arrays *)
let array_len : 'a array cde -> int cde = function
   | {vars;inits;desc=Varr(_,_,len)} -> {vars;inits;desc=Exp(tint,len)}
   | _ -> assert false

let array_set : 'a array cde -> int cde -> 'a cde -> unit cde = fun arr i v ->
  merge3 arr i v @@ fun vars inits arr i v ->
    match (arr,i,v) with
    | (Varr(_,ar,_), Exp(_,i), Exp (_,v)) ->
        let inits = Seq.push (assign (Index(Var ar,i)) v) inits in
        {vars;inits;desc=Stm}
    | _ -> assert false

let array_get' : 'a array cde -> int cde -> 'a cde = fun arr i -> 
   merge arr i @@ fun[@warning "-8"] vars inits (Varr (t,ar,_)) (Exp (_,i)) ->
   {vars;inits;desc=Exp(t,Index(Var ar,i))}

(* It makes sense to combine it with letl *)
(* XXX Optimization: if the index expression is a constant, or if it
   is a reference to a variable that is not subject to :=
   (wasn't created by newref), then don't make a let-binding
 *)
let array_get : 'a array cde -> int cde -> ('a cde -> 'w cde) -> 'w cde
    = fun arr i k -> 
      merge arr i @@ 
      fun[@warning "-8"] vars inits (Varr (t,ar,_)) (Exp (_,i)) ->
        let iexp = {vars;inits;desc=Exp(t,Index(Var ar,i))} in
        letl iexp k

(* new uninitialized array, of the base type
*)
let new_uarray : 'a tbase -> int -> ('a array cde -> 'w cde) -> 'w cde =
  fun t n k -> 
    let aname = genname "a" in
    let def = ((aname,{specs=[];typ=Tarray (n,cbtype t)}),Init_none) in
    let r = k (of_desc @@ Varr(t,aname,Const(Const_num (string_of_int n)))) in
    {r with vars = Seq.push_first def r.vars}

(* initialized array, immediately bound to a variable
   Must be of a base type
*)
let new_array  : 'a tbase -> 'a cde array -> ('a array cde -> 'w cde) -> 'w cde
  = fun t iarr k ->
    let n = Array.length iarr in
    assert Stdlib.(n>0);
    let (vars,inits) =
      Array.fold_right (fun {vars;inits} (accv,acci)  ->
        (Seq.merge vars accv, Seq.merge inits acci)) iarr 
        (Seq.empty,Seq.empty) in
    let arr_inits = 
      Array.map (function {desc=Exp(_,exp)} -> exp | _ -> assert false) iarr |>
      Array.to_list in
    let aname = genname "a" in
    let def = ((aname,{specs=[];typ=Tarray (n,cbtype t)}),
                Init_many arr_inits) in
    let vars = Seq.push def vars in
    seq {vars;inits;desc=Stm} @@
       k (of_desc @@ Varr(t,aname,Const(Const_num (string_of_int n))))

(* Simple i/o, useful for debugging. Newline at the end *)
let print_int   : int cde   -> unit cde = fun {vars;inits;desc=Exp(_,exp)} ->
  let inits = 
    Seq.push (CALL (Var "printf",[Const (Const_string "%ld\n");
                                  Cast({typ=Tlong;specs=[]},exp)])) inits in
  {vars;inits;desc=Stm}
let print_float   : float cde   -> unit cde =fun {vars;inits;desc=Exp(_,exp)} ->
  let inits = 
    Seq.push (CALL (Var "printf",[Const (Const_string "%g\n");exp])) inits in
  {vars;inits;desc=Stm}

(* Control operators *)
(* Be sure to use proper expressions, if they can be executed zero or
   more than one times
 *)

let to_block' : unit cde -> block = fun {vars;inits} ->
  {blabels=[]; bdefs=Seq.to_list vars; bstmts=Seq.to_list inits}

let to_block : unit cde -> statement = fun c -> BLOCK (to_block' c)

(* Perhaps separate if_ expression from if-statement *)
(* XXX Cannot use for-statements within bt & bf
** when it becomes a C conditional operator (_?_:_).
** Thus, the C backend definitely has a little bit
** regulation on the grammar.
*)
let cond : type a. bool cde -> a cde -> a cde -> a cde = 
 fun ({vars;inits;desc=Exp(_,ecnd)} as cnd) bt bf ->
  match (bt,bf) with
  | ({desc=Stm},{desc=Stm}) ->
      let inits = Seq.push (IF(ecnd,to_block bt,to_block bf)) inits in
      {vars;inits;desc=Stm}
  | (bt,bf) -> merge3 cnd (proper_exp bt) (proper_exp bf) @@
      fun vars inits (Exp(_,ecnd)) bt bf ->
      match (bt,bf) with
      | (Exp(t,et),Exp(_,ef)) ->
          {vars;inits;desc=Exp(t,Cond(ecnd,et,ef))}
      | _ -> assert false

let if_  : bool cde -> unit cde -> unit cde -> unit cde = cond
let if1  : bool cde -> unit cde -> unit cde = 
 fun {vars;inits;desc=Exp(_,ecnd)} bt ->
   let inits = Seq.push (IF(ecnd,to_block bt,NOP)) inits in
   {vars;inits;desc=Stm}

let for_ : int cde ->           (* exact lower bound *)
           int cde ->           (* exact upper bound *)
           ?guard:bool cde ->   (* possibly a guard, terminate when false *)
           ?step:int cde ->     (* step *)
           (int cde -> unit cde) -> unit cde = 
  fun lwb ({desc=Exp(_,eupb)} as upb) ?guard ?(step=int 1) body ->
  let i = genname "i" in
  let idef = ((i,cbtype tint),Init_none) in
  let guardi = Binary (LE,Var i,eupb) in
  let guard = match guard with
              | None   -> exp guardi tbool
              | Some e -> exp guardi tbool && (proper_exp e) in
  merge4 lwb upb guard step @@ 
  fun vars inits (Exp (_,lwb)) _ (Exp (_,guard)) (Exp (_,step)) ->
    let vars = Seq.push idef vars in
    let for_stm = FOR(assign_var i lwb,
                      guard,
                      Stdlib.(if step = Const (Const_num "1") 
                       then UNMOD (POSINCR,Var i)
                       else BIMOD (ADD_ASSIGN,Var i,step)), 
                      to_block (body (exp (Var i) tint))) in
    let inits = Seq.push for_stm inits in
    {vars;inits;desc=Stm}

let while_ : bool cde -> unit cde -> unit cde = fun cnd body ->
  match proper_exp cnd with
    {vars;inits;desc=Exp(_,goon)} ->
      let inits = Seq.push (WHILE(goon,to_block body)) inits in
      {vars;inits;desc=Stm}


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
    ('a -> unit cde) ->        (* cloop's continuation *)
    bool cde option ->         (* possibly a guard *)
    (('a -> unit cde) -> unit cde) ->   (* body, in CPS, which may exit
                                           without calling its continuation
                                         *)
    unit cde = 
    fun k bp body ->
      newref (bool true) @@ fun again ->
      let {vars;inits;desc=Exp(_,bp)} = 
        match bp with
        | None -> dref again
        | Some bp -> dref again && bp
      in
      let inits =
        Seq.push (DOWHILE (bp,
                           to_block @@ body (fun x -> seq (k x) 
                                                     (again := bool false)))) @@
        inits in
      {vars;inits;desc=Stm}

(* Inquiries. They are best effort *)
(* Is value statically known: known at code-generation time *)
let is_static : 'a cde -> bool = fun _ -> false
(* Is value dependent on something introduced by the library itself?
   For example, dependent on letl identifier, etc.
   It may always return true.
 *)
let is_fully_dynamic : 'a cde -> bool = fun x -> Stdlib.not (is_static x)


(* Top-level functions *)

let nullary_fun' : type a. ?name:string -> a cde -> declaration = 
  fun ?(name="fn") c ->
  let (ctyp,block) = match c with
  | {desc=Stm} -> ({specs=[];typ=Tvoid},to_block' c)
  | {vars;inits;desc=Exp(t,e)} -> 
      (cbtype t, to_block' @@
       {vars; inits=Seq.push (RETURN e) inits; desc=Stm})
  | _ -> assert false
  in 
  FUNDEF(ctyp,name,[],block)

let nullary_fun : ?name:string -> Format.formatter -> 'a cde -> unit =
  fun ?name ppf c -> nullary_fun' ?name c |> C_pp.pr_decl ppf 

(* Run a procedure and capture its standard output, which is
   returned as an input channel (which needs to be closed).
   Typically that channel is open on a temp file, which is automatically
   removed when the channel is closed.
   We use Scanf.Scanning.in_channel for uniformity with Trx module:
   such channels can be opened on strings, buffers, etc.
 *)
let run_capture_output : unit cde -> Scanf.Scanning.in_channel = fun c ->
  let (cdename,cdechan) = Filename.open_temp_file ~temp_dir:"/tmp" "cde" ".c" in
  Format.fprintf (Format.formatter_of_out_channel cdechan)
    "#include <stdio.h>\n#include <stdlib.h>\n
     #include <math.h>\n#include <stdbool.h>\n
     %a\n@."
     C_pp.pr_decl
     (FUNDEF({specs=[];typ=Tint},
             "main",[],       (* main must have the return type just int *)
      to_block' 
       {c with inits=Seq.push (RETURN (Const (Const_num "0"))) c.inits}));
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

let run_output_to_command : unit cde -> string -> unit = fun code comanndname ->
  (* Print the code to a temporary file *)
  let (cdename,cdechan) = Filename.open_temp_file ~temp_dir:"/tmp" "cde" ".c" in
  Format.fprintf (Format.formatter_of_out_channel cdechan)
    "#include <stdio.h>\n#include <stdlib.h>\n
    #include <math.h>\n#include <stdbool.h>\n
    %a\n@."
    C_pp.pr_decl
    (FUNDEF({specs=[];typ=Tint},
            "main",[],       (* main must have the return type just int *)
      to_block' 
      {code with inits=Seq.push (RETURN (Const (Const_num "0"))) code.inits}));
  close_out cdechan;
  (* Invoke the code, then redirect its output to the command *)
  let rc = Printf.kprintf Sys.command 
      "cd /tmp && gcc -Wall -lm %s && ./a.out | (%s > /dev/null 2>&1)" cdename comanndname in
  if Stdlib.(rc = 0) then begin
    (* For debug: `print_endline cdename` *)
    Sys.remove cdename
  end
  else failwith "compilation errors; examine the files"
  
(* Useful mostly for debugging. It adds printf and 
   returns the printf-completed statement and the deserializer.
 *)
let write_res : type a. a cde -> (unit cde * (string -> a)) = function
  | {vars;inits;desc=Exp(t,exp)} ->
      let (fmt,exp,(deser : string -> a)) = match t with
      {printf_fmt;printf_cast;deser} -> (printf_fmt, printf_cast exp, deser)
      in
      let inits = 
      Seq.push (CALL (Var "printf",[Const (Const_string fmt);exp])) inits in
      ({vars;inits;desc=Stm}, deser)
  | _ -> failwith "write_res: only base types"

(* Only for base types *)
let run : type a. a cde -> a = fun c ->
  let (c,deser) = write_res c in
  let cin = run_capture_output c in
  let r = Scanf.bscanf cin "%s@\n" deser in Scanf.Scanning.close_in cin; r

let one_array_fun' : type a b. ?name:string -> 
  a tbase -> (a array cde -> b cde) -> declaration = 
  fun ?(name="fn") tp bodyf ->
  let a = genname "aa" in
  let l = genname "al" in
  let arr = of_desc @@ Varr (tp,a,Var l) in
  let body = bodyf arr in
  let (ctyp,block) = match body with
  | {desc=Stm} -> ({specs=[];typ=Tvoid},to_block' body)
  | {vars;inits;desc=Exp(t,e)} -> 
      (cbtype t, to_block' @@
       {vars; inits=Seq.push (RETURN e) inits; desc=Stm})
  | _ -> assert false
  in 
  FUNDEF(ctyp,name,[(a,{specs=[S_const];typ=Tptr (cbtype tp)});
                    (l,cbtype ~specs:[S_const] tint)],block)

let one_array_fun : ?name:string -> Format.formatter -> 
  'a tbase -> ('a array cde -> 'b cde) -> unit =
  fun ?name ppf tp bodyf -> one_array_fun' ?name tp bodyf |> C_pp.pr_decl ppf 

let two_array_fun' : type a b c. ?name:string -> 
  a tbase * b tbase -> (a array cde * b array cde -> c cde) -> declaration = 
  fun ?(name="fn") (tp1,tp2) bodyf ->
  let a1 = genname "a1a" in
  let l1 = genname "a1l" in
  let arr1 = of_desc @@ Varr (tp1,a1,Var l1) in
  let a2 = genname "a2a" in
  let l2 = genname "a2l" in
  let arr2 = of_desc @@ Varr (tp2,a2,Var l2) in
  let body = bodyf (arr1,arr2) in
  let (ctyp,block) = match body with
  | {desc=Stm} -> ({specs=[];typ=Tvoid},to_block' body)
  | {vars;inits;desc=Exp(t,e)} -> 
      (cbtype t, to_block' @@
       {vars; inits=Seq.push (RETURN e) inits; desc=Stm})
  | _ -> assert false
  in 
  FUNDEF(ctyp,name,
         [(a1,{specs=[S_const];typ=Tptr (cbtype tp1)});
          (l1,cbtype ~specs:[S_const] tint);
          (a2,{specs=[S_const];typ=Tptr (cbtype tp2)});
          (l2,cbtype ~specs:[S_const] tint);
         ],block)

let two_array_fun : ?name:string -> Format.formatter -> 
  'a tbase * 'b tbase -> ('a array cde * 'b array cde -> 'c cde) -> unit =
  fun ?name ppf tps bodyf -> two_array_fun' ?name tps bodyf |> C_pp.pr_decl ppf 

(* More general function/procedure generation *)
(* Currently, when the function takes an array there must be an earlier
   argument of type int that tells the length of the array.
   What if we want to generate a function where the array length is in
   a later argument? Currently, it is not supported. If needed,
   we need to define a special type 'a pre_array for just array
   name argument, and a special function
   assemble_array : 'a pre_array -> int cde -> 'a array cde
   that associates the pre-array with the array length
 *)

(* used internally while building the function declaration *)
type 'a fun_decl = ctype * typedname list * block

let gen_fun : type a. a cde -> a fun_decl = function
  | ({desc=Stm} as body) -> ({specs=[];typ=Tvoid},[],to_block' body)
  | {vars;inits;desc=Exp(t,e)} -> 
      (cbtype t, [], to_block' @@
       {vars; inits=Seq.push (RETURN e) inits; desc=Stm})
  | _ -> assert false

let arg_base : 'a tbase -> ('a cde -> 'b fun_decl) -> ('a -> 'b) fun_decl =
  fun bt bodyf ->
    let n = genname "n" in
    let arg = of_desc @@ Exp (bt, Var n) in
    let (ct,args,block) = bodyf arg in
    (ct, (n,cbtype ~specs:[S_const] bt)::args,block)

(* the second argument is an expression of int type that is a variable
   that contains the length of the array
 *)
let arg_array : ?mutble:bool -> 
  'a tbase -> int cde -> ('a array cde -> 'b fun_decl) -> 
  ('a array -> 'b) fun_decl =
  fun ?(mutble=false) bt len_exp bodyf ->
    let a = genname "a" in
    let l = match len_exp with
    | {desc=Exp (_, Var l)} -> l
    | _ -> failwith "arg_array: len_exp must be a var reference"
    in
    let arg = of_desc @@ Varr (bt,a,Var l) in
    let (ct,args,block) = bodyf arg in
    (ct, (a,{specs=(if mutble then [] else [S_const]);
             typ=Tptr (cbtype bt)})::args,block)

let make_fun : ?name:string -> Format.formatter -> 'a fun_decl -> unit =
  fun ?(name="fn") ppf (ctyp,args,block) ->
  FUNDEF(ctyp,name, args, block) |> C_pp.pr_decl ppf 


let cde_app1 : type a. string -> 'b tbase -> a cde -> 'b cde =
  fun fname ret_typ ->
    let ( && ) = Stdlib.( && ) in
    function
    | {vars;inits;desc=Exp(_,exp)} ->
      let desc = Exp(ret_typ,Call (Var fname,[exp])) in
      {vars;inits;desc}
    | {vars; inits; desc=Stm}
        when (Seq.is_empty vars && Seq.is_empty inits) -> (* when unit *)
      of_desc (Exp(ret_typ,Call (Var fname,[Nothing])))
    | _ -> failwith "not supported"

let cde_app2 : type a b. string -> 'c tbase -> a cde -> b cde -> 'c cde =
  fun fname ret_typ x y ->
    match (x,y) with
    | {desc=Exp(_,exp1)},{desc=Exp(_,exp2)} ->
      merge x y (fun vars inits _ _ ->
        let desc = Exp(ret_typ,Call (Var fname,[exp1;exp2])) in
        {vars;inits;desc}
      )
    | _ -> failwith "not supported"
