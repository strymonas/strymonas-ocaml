(* StreamIt emulator *)

(* This is a simplest StreamIt interpreter, with no regard to efficiency and,
   specifically, fusion.

   The purpose is to be able to step-wise verify StreamIt and Strymonas
   pipelines.

   Since StreamIt filters are written in imperative style, we retain this
   style. The goal is to be able to run StreamIt code almost as it is.
*)

(* A pair of input and output streams, as lists *)
module type iostream = sig
  type i                                (* input stream element type *)
  type o                                (* output stream element type *)
  exception Finished            (* thrown when the input stream is finished *)
  val push : o -> unit
  val pop : unit -> i           (* the latter two may throw the exception *)
  val peek : int -> i
end

type ('i,'o) iostream = (module iostream with type i = 'i and type o = 'o)

type ('i,'o) filter = 
    {prework: ('i,'o) iostream -> unit;
     work:    ('i,'o) iostream -> unit;
   }

(* A very naive, but clearly correct, implementation of iostream 
   Essentially, the executable specification
*)
module IOList(S: sig type i type o val il : i list end) = struct
  type i = S.i
  type o = S.o
  let input  : i list ref = ref S.il
  let output : o list ref = ref []      (* in reverse order *)
  exception Finished            (* thrown when the input stream is finished *)

  let get_output : unit -> o list = fun () -> List.rev !output

  let push : o -> unit  = fun x -> output := x :: !output
           (* the latter two may throw the exception *)
  let pop : unit -> i = fun () -> match !input with
  | [] -> raise Finished
  | h :: t -> input := t; h

  let peek : int -> i = fun x -> match List.nth !input x with
  | r -> r
  | exception (Failure _) -> raise Finished
end

(* Execute a StreamIt filter on a given input *)
let run_filter : type i o. (i,o) filter -> i list -> o list = fun fl il ->
  let module IOS = 
    IOList(struct type nonrec i = i type nonrec o = o let il = il end) in
  fl.prework (module IOS);
  begin try while (true) do fl.work (module IOS) done
        with IOS.Finished -> ()
  end;
  IOS.get_output ()

(* run several filters in (split duplicate; join roundrobin;) fashion.
   All the filters should have the same window and the same production rate.
 *)

(* All lists have to have the same length *)
let rec interleave : 'a list list -> 'a list = function
  | [] -> []
  | [] :: t -> if List.for_all ((=) []) t then [] else
    failwith "interleave: lists should have the same length"
  | (h::t) :: tt -> h :: interleave (tt @ [t])

(*
let _ = interleave [[1;2];[3;4];[5;6]]
let _ = interleave [[1;2];[3;4;10];[5;6]]
*)

let duplicate_rr : type i o. (i,o) filter list -> i list -> o list = 
  fun fls il ->
    List.map (fun fl -> run_filter fl il) fls |> interleave

let duplicate_rr' : type i o. (i list -> o list) list -> i list -> o list = 
  fun fls' il ->
    List.map (fun fl' -> fl' il) fls' |> interleave
;;

