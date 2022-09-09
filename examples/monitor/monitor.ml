(*
cd sample/monitor
rlwrap metaocaml 

#require "str";;
#load "monitor_meta.cma";;
open Monitor;;
#require "archimedes";;

Runcode.run code1;;
*)

open Stream_cooked
open Util
open Unix

(* Actually, we should use inotify. *)
let of_file file =
  let init k = .<let ic = open_in file in .~(k .<ic>.)>. in
  let rec step s ondone k = .<
    try
      let line = input_line .~s in
      .~(k .<line>.)
    with e ->
      (* by polling *)
      Unix.sleep 5;
    >.
  in
  unfold step init;;

let of_process cmd =
  let init k = .<let ic = open_process_in cmd in .~(k .<ic>.)>. in
  let rec step s ondone k = .<
    try
      let line = input_line .~s in
      .~(k .<line>.)
    with e ->
      (* by polling *)
      Unix.sleep 5;
    >.
  in
  unfold step init;;

let plotter str n = .<
  let open Archimedes in
  let vp = init [] in
  Axes.box vp;
  set_color vp Color.orange_red;
  .~(str
     |> fold (fun z e -> .<update .~z .~e;
                           plot vp .~z;
                           .~z>.) .<cnts n>.);
>.


(* Streams *)
(* for macOS *)
let poweredon_time_str =
  of_file "/private/var/log/install.log"
  |> filter (fun e -> .<contains .~e "[569]">.)
  |> filter (fun e -> .<contains .~e "powered on">.)
  |> map (fun e -> .<String.sub .~e 0 22>.)
  |> map (fun e -> .<split_time .~e @@ fun tm_year tm_mon tm_mday tm_hour tm_min tm_sec _ -> 
                                       snd @@ mktime {tm_sec;tm_min;tm_hour;tm_mday;tm_mon;tm_year;
                                                      tm_wday=0;
                                                      tm_yday=0;
                                                      tm_isdst=false}>.)

let poweredon_hour_str =
  poweredon_time_str
  |> map (fun e -> .<.~e.tm_hour>.)

let poweredon_week_str =
  poweredon_time_str
  |> map (fun e -> .<.~e.tm_wday>.)

let multitouch_time_str =
  of_process "log stream"
  |> filter (fun e -> .<contains .~e "MultitouchHID">.)
  |> map (fun e -> .<String.sub .~e 0 22>.)
  |> map (fun e -> .<split_time_lastf .~e @@ fun _ _ _ _ _ s -> s>.)


(* Processes *)
let code1 = plotter poweredon_hour_str 24
let code1' = plotter poweredon_week_str 7
let code2 = 
  multitouch_time_str
  |> fold (fun z e -> .<Printf.printf "time: %2d/cnt: %d\n" (Pervasives.truncate .~e) .~z;
                        Pervasives.flush Pervasives.stdout;
                        .~z + 1>.) .<1>.


(* Export *)
open Codelib;;
open Format;;
let cde = close_code code1
let oc = open_out "main.ml"
let ppf = formatter_of_out_channel oc
let () = format_code ppf cde
let () = fprintf ppf "%!"