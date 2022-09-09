(* Strymonas also allows streams that carry not mere target
   code values but also composite values such as tuples, whose
   structure is known at code-generation time. Such values should be
   accompanied by a descriptor.

*)

module type cde        = module type of Cde

module type desc = sig
  type 'a cde
  (* The two arguments of (coll,collref) desc are:
     coll: the type of the collection
     collref : the type to the mutable reference collection
     Currently we do not provide mapping between collections.
     If we do provide it, we need an index type: see below for an example.
  *)
  type (_,_) desc =
    | Single : ('a cde, 'a ref cde) desc
    | Tuple  : ('a cde * 'b cde, 'a ref cde * 'b ref cde) desc
        (* can be extended and generalized to triples, etc. and pairs *)
end

module Desc(C:cde) = struct
  type 'a cde = 'a C.cde
  type (_,_) desc =
    | Single : ('a cde, 'a ref cde) desc
    | Tuple  : ('a cde * 'b cde, 'a ref cde * 'b ref cde) desc
  let newref : type c cref w. 
        (c,cref) desc -> c -> (cref -> w cde) -> w cde =
    fun d c k ->
      match d with
      | Single -> C.newref c k
      | Tuple  ->
          let (x,y) = c in
          C.newref x @@ fun xr ->
          C.newref y @@ fun yr ->
          k (xr,yr)
  let dref : type c cref. (c,cref) desc -> cref -> c =  fun d cr ->
    match d with
    | Single -> C.dref cr
    | Tuple  ->
        let (xr,yr) = cr in
        C.(dref xr,dref yr)
  let set : type c cref. (c,cref) desc -> cref -> c -> unit cde = fun d cr c ->
    match d with
    | Single -> C.(cr := c)
    | Tuple  ->
        let (xr,yr) = cr and (x,y) = c in
        C.(seq (xr := x) (yr := y))

  (* As for array, this interface prefer multiple host arrays to arrays of structs in SQL term.
  ** - https://docs.oracle.com/en/database/oracle/oracle-database/21/lnpcc/host-arrays.html#GUID-861203A7-309A-4A33-A6B8-89B10B36E0C4
  ** - https://stackoverflow.com/questions/17924705/structure-of-arrays-vs-array-of-structures
  *)
end


(*
module type coll = sig
  type idx                              (* representation of coll's content *)
  module C : cde
  type 'a cde = 'a C.cde
  type coll
  type collref
  val newref : coll -> (collref -> 'w cde) -> 'w cde
  val dref : collref -> coll
  val dset : collref -> coll -> unit cde
end

type ('i,'c) desc = (module (coll with type idx='i and type coll='c))

module Single(C:cde)(S:sig type idx end) : 
    (coll with module C=C and type idx=S.idx and type coll=S.idx C.cde) = struct
  type idx = S.idx
  module C = C
  type 'a cde = 'a C.cde
  type coll = idx cde
  type collref = idx ref cde
  let newref : coll -> (collref -> 'w cde) -> 'w cde = C.newref
  let dref : collref -> coll = C.dref
  let dset : collref -> coll -> unit cde = C.(:=)
end

module Tuple(C:cde)(S:sig type i1 type i2 end) : 
    (coll with module C=C and type idx=S.i1 * S.i2 and 
          type coll= S.i1 C.cde * S.i2 C.cde) = struct
  type idx = S.i1 * S.i2
  module C = C
  type 'a cde = 'a C.cde
  type coll = S.i1 cde * S.i2 cde
  type collref = S.i1 ref C.cde * S.i2 ref C.cde
  let newref : coll -> (collref -> 'w cde) -> 'w cde = fun (x,y) k ->
    C.newref x @@ fun xr ->
    C.newref y @@ fun yr ->
    k (xr,yr)
  let dref : collref -> coll = fun (xr,yr) -> (C.dref xr, C.dref yr)
  let dset : collref -> coll -> unit cde = fun (xr,yr) (x,y) ->
    C.(seq (xr := x) (yr := y)) 
end

module Desc(C:cde) = struct
  let single : type i. unit -> (i,i C.cde) desc = fun () ->
      let module M = Single(C)(struct type idx=i end) in
      (module M)
  let tuple : type ix1 ix2. unit -> (ix1*ix2,ix1 C.cde * ix2 C.cde) desc = 
    fun () ->
      let module M = Tuple(C)(struct type i1=ix1 type i2=ix2 end) in
      (module M)
end

*)

(* A different approach, reminiscent of HList 
   The descriptor is bundled with the value
type 'a single
module type coll = sig
  type 'a el
  type (_,_) desc = 
    | Unit  : (unit,unit) desc
    | One   : ('a single,'a el) desc
    | Tuple : ('a,'a1) desc -> ('b * 'a, 'b el * 'a1) desc
  type 'a coll = Coll : ('a,'e) desc * 'e -> 'a coll
  val unit    : unit coll
  val single  : 'a el -> 'a single coll
  val from_single : 'a single coll -> 'a el
  val ascribe : ('a,'e) desc -> 'e -> 'a coll
end

module Coll(E: sig type 'a t end) = struct
  type 'a el = 'a E.t
  type (_,_) desc = 
    | Unit  : (unit,unit) desc
    | One   : ('a single,'a el) desc
    | Tuple : ('a,'a1) desc -> ('b * 'a, 'b el * 'a1) desc
  type 'a coll = Coll : ('a,'e) desc * 'e -> 'a coll
  let unit    : unit coll = Coll (Unit,()) 
  let single  : 'a el -> 'a single coll = fun x -> Coll (One,x)
  let from_single : type a. a single coll -> a el = function Coll (One,x) -> x
  let ascribe : ('a,'e) desc -> 'e -> 'a coll = fun d x -> Coll (d,x)
end

(* Mapping, between element representations. Element type _index_
  should be preserved though (which means that the length is also
  preserved)
*)
module CMAP(S:coll)(T:coll) = struct
  type ftor     = {f: 'a. 'a S.el -> 'a T.el}
  type 'w ftor_cps = {fcps: 'a. 'a S.el -> ('a T.el -> 'w) -> 'w}
  let rec map : type a. ftor -> a S.coll -> a T.coll = fun {f} -> function
    | S.Coll (Unit,_) -> T.Coll (Unit,())
    | S.Coll (One,x)  -> T.Coll (One,f x)
    | S.Coll (Tuple d,(h,t)) -> 
        let T.Coll (d',t') = map {f} (S.Coll (d,t)) in
        T.Coll (Tuple d', (f h, t'))
  let rec map_cps : type a w. 
    w ftor_cps -> a S.coll -> (a T.coll -> w) -> w = fun {fcps} sc k -> 
      match sc with
      | S.Coll (Unit,_) -> k @@ T.Coll (Unit,())
      | S.Coll (One,x)  -> fcps x @@ fun x' -> k @@ T.Coll (One,x')
      | S.Coll (Tuple d,(h,t)) -> 
          fcps h @@ fun h' ->
          map_cps {fcps} (S.Coll (d,t)) @@ function T.Coll (d',t') ->
            k @@ T.Coll (Tuple d', (h', t'))
end
*)


