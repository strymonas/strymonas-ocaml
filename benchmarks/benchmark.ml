open Runcode
open Benchmark_types
open Util

let perfS : string -> 'a code -> ('a code -> 'b code) -> 'b benchmark_options -> 'b code =
  fun benchmark init f options ->
    let reps = options.repetitions in
    let final = options.final_f in
    assert (reps>1);

    .<let[@inline never] fn = fun x -> .~(f .<x>.) in
      let x = .~init in
      let ret = ref (fn x) in
      let measurements = Array.make reps 0.0 in
      (* for i = 0 to reps-1 do
         ignore (Sys.opaque_identity (fn x)) (* warming up *)
      done; *)
      for i = 0 to reps-1 do
        Gc.compact();
        (* XXX Sys.time is getrusage, may be not so good:
               https://github.com/ocaml/ocaml/blob/b4c5d7a55d9ec25693ba741a613d81f2c3ef66bc/runtime/sys.c#L512 *)
        let start_time = Sys.time () in
        let _ = Sys.opaque_identity (fn x) in
        let end_time = Sys.time () in
        let elapsed_time = (end_time -. start_time) *. 1000.0 in
        measurements.(i) <- elapsed_time
      done;
      Printf.printf "%-30s %10.1f %10.1f %5.1f   ms/op\n%!"
         benchmark (mean measurements) (mean_error 0.95 measurements) (standard_deviation measurements);
      
      .~(final .<!ret>.);
      !ret>.;;

let perfS2 : string -> 'a code -> 'a code -> (('a code* 'a code) -> 'b code) -> 'b benchmark_options -> 'b code =
  fun benchmark init1 init2 f options ->
    let reps = options.repetitions in
    let final = options.final_f in
     assert (reps>1);

    .<let[@inline never] fn (x,y) = .~(f (.<x>.,.<y>.) ) in
      let x = .~init1 in
      let y = .~init2 in
      let ret = ref (fn (x,y)) in
      let measurements = Array.make reps 0.0 in
      (* for i = 0 to reps-1 do
         ignore (Sys.opaque_identity (fn (x,y))) (* warming up *)
      done; *)
      for i = 0 to reps-1 do
         Gc.compact();
         let start_time = Sys.time () in
         let _ = Sys.opaque_identity (fn (x,y)) in
         let end_time = Sys.time () in
         let elapsed_time = (end_time -. start_time) *. 1000.0 in
         measurements.(i) <- elapsed_time
      done;
      Printf.printf "%-30s %10.1f %10.1f %5.1f   ms/op\n%!"
         benchmark (mean measurements) (mean_error 0.95 measurements) (standard_deviation measurements);
      
      .~(final .<!ret>.);
      !ret>.;;

let write_code : string -> 'a code -> unit = fun file_name c ->
   (* ===== make sure the code is closed ===== *)
   (* let start_time = Sys.time () in *)
   let cde = close_code c in
   (* let end_time = Sys.time () in *)
   (* Printf.printf "closing code took: %5.1f ms\n%!" ((end_time -. start_time) *. 1000.0); *)
   let cout = open_out file_name in
   let ppf = Format.formatter_of_out_channel cout in
   (* ===== print code ===== *)
   (* let start_time = Sys.time () in *)
   let () = (format_code ppf cde; Format.fprintf ppf "%!") in
   (* let end_time = Sys.time () in *)
   (* Printf.printf "emitting code took: %5.1f ms\n%!" ((end_time -. start_time) *. 1000.0); *)
   close_out cout

(* Use Ctrl-z to interrupt running *)
let run_natively : ?compiler:string -> ?save:bool -> 'a code -> unit =
   fun ?(compiler:string="ocamlopt -O2 -unsafe -nodynlink util.cmx") ?(save:bool=false) c ->
      let fname = Filename.(concat (get_temp_dir_name()) "gen.ml") in
      write_code fname c;
      (* let start_time = Sys.time () in *)
      let retc = Sys.command (compiler ^ " " ^ fname) in
      (* let end_time = Sys.time () in *)
      (* Printf.printf "compilation took: %5.1f ms\n%!" ((end_time -. start_time) *. 1000.0); *)
      if retc = 0 then begin
         ignore (Sys.command "./a.out");
         begin
            if save then
              print_endline fname
            else
              Sys.remove fname
          end;
         Sys.remove "./a.out"
      end;;

let run_script : ?compiler:string -> 'a code array -> unit =
   fun ?compiler arr -> 
      Printf.printf "%-30s %10s %10s %5s %7s\n%!" "Benchmark" "Mean" "Mean-Error" "Sdev" "Unit";
      Array.iter (fun c -> run_natively ?compiler c) arr
;;
