let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try 
       ignore (Str.search_forward re s1 0); 
       true
    with Not_found -> false


let cnts n = Array.make n 0.

let update a h =
  let open Array in
  set a h @@ 1. +. get a h

let plot vp a =
  let open Archimedes in
  (* Array.y vp a ~style:(`Bars 0.6); *)
  Array.y vp a ~fill:true ~fillcolor:Color.orange_red ~style:(`Bars 0.6);
  show vp

let split_time time f =
   Scanf.sscanf time "%d-%d-%d %d:%d:%d+%d" @@ f

let split_time_lastf time f =
   Scanf.sscanf time "%d-%d-%d %d:%d:%f" @@ f