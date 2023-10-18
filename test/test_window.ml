(* testing windowing *)

(*
#directory "../lib";;
#directory "../lib/backends/Trx";;
#directory "../lib/backends/C";;
#load "stream.cma";;
*)


module type cde = module type of Cde_top

module Utils(C: cde) = struct
  let check msg cnv v exp =
    print_endline msg;
    let r = C.run exp in
    if r = v then () else
    failwith @@ "expected: " ^ cnv v ^ "; but found " ^ cnv r

  let check_int msg v exp = check msg string_of_int v exp
  let check_float msg v exp = check msg string_of_float v exp

  let show_list : ('a -> string) -> 'a list -> string = fun shel lst ->
    let open Format in
    fprintf str_formatter "@[<h>[%a]@]"
     (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ";";
                                      pp_print_space fmt ())
        (fun fmt x -> shel x |> pp_print_string fmt)) lst;
    flush_str_formatter ()

  let check_list reader printer msg v exp =
    print_endline msg;
    let c = C.run_capture_output exp in
    let rec loop acc = 
      match reader c :: acc with
      | exception End_of_file -> Scanf.Scanning.close_in c; List.rev acc
      | acc -> loop acc 
    in 
    let r = loop [] in
    let v = List.map printer v in
    if r = v then () else
    Printf.kprintf failwith "expected: %s but found %s"
      (show_list Fun.id v) (show_list Fun.id r)

  let reader_single c = Scanf.bscanf c "%s\n" Fun.id
  let reader_tuple c  = 
    Scanf.bscanf c "%s\n%s\n" (fun x y -> x ^ "," ^ y)

  let check_int_list = check_list reader_single string_of_int
  let check_intint_list = check_list reader_tuple 
      (fun (x,y) -> Printf.sprintf "%d,%d" x y)

  let even x = C.(x mod int 2 = int 0)
  let square x = C.(x * x)
end

module CTrx = struct
  include Pk_cde.Make(Trx_code)
end

module CC = struct
  include Pk_cde.Make(C_cde)
end

module Test2(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Window_fn.Make(C)(Raw)
  open Utils(C)

  let () = Printf.printf "\nSize 2 test, %s backend\n" C.ident

  let test1 =
    let (module Win) = make_window C.tint 2 1 in
    of_int_array [|1;2;3;4;5;6;7|] 
    |> Win.make_stream 
    |> Raw.map_raw (Win.reduce C.( + )) 
    |> iter C.print_int

  let () = check_int_list "test1" [3;5;7;9;11;13] test1

  let test1p =
    let (module Win) = make_window C.tint 2 1 in
    of_int_array [|1;2;3;4;5;6;7|] 
    |> Win.make_stream ~padding:true
    |> Raw.map_raw (Win.reduce C.( + )) 
    |> iter C.print_int

  let () = check_int_list "test1p" [1;3;5;7;9;11;13] test1p

  let test2 =
    let (module Win) = make_window C.tint 2 2 in
    of_int_array [|1;2;3;4;5;6;7|] 
    |> Win.make_stream 
    |> Raw.map_raw (Win.reduce C.( + )) 
    |> iter C.print_int

  let () = check_int_list "test2" [3;7;11] test2

  let test2p =
    let (module Win) = make_window C.tint 2 2 in
    of_int_array [|1;2;3;4;5;6;7|] 
    |> Win.make_stream ~padding:true 
    |> Raw.map_raw (Win.reduce C.( + )) 
    |> iter C.print_int

  let () = check_int_list "test2p" [1;5;9;13] test2p

  let test3 =
    let (module Win) = make_window C.tint 2 1 in
    of_int_array [|1;2;3;4;5;6;7|] 
    |> Win.make_stream 
    |> Raw.map_raw (Win.reduce C.( - )) 
    |> iter C.print_int

  let () = check_int_list "test3" [1;1;1;1;1;1] test3

  let test3' =
    let (module Win) = make_window C.tint 2 1 in
    of_int_array [|1;2;3;4;5;6;7|] 
    |> Win.make_stream 
    |> Raw.map_raw (Win.dot C.tint C.int [|1;-1|] C.( + ) C.( * )) 
    |> iter C.print_int

  let () = check_int_list "test3'" [1;1;1;1;1;1] test3'

  let test4 =
    let (module Win) = make_window C.tint 2 1 in
    of_int_array [|1;2;3;4;5;6;7|] 
    |> Win.make_stream 
    |> Raw.map_raw (Win.dot C.tint C.int [|-1;1|] C.( + ) C.( * )) 
    |> iter C.print_int

  let () = check_int_list "test4" [-1;-1;-1;-1;-1;-1] test4

  let test5 = 
    let module F32 = C.F32 in
    let (module Win) = make_window F32.tbase 2 1 in
    of_static_arr F32.tbase (fun x -> float_of_int x |> F32.lit)
      [|1;2;3;4;5;6;7|] 
    |> Win.make_stream 
    |> Raw.map_raw F32.(Win.dot tbase lit [|-1.;1.|] ( +. ) ( *. )) 
    |> iter F32.print

  let () = check_list reader_single 
      (fun x -> 
        if C.ident = "C" then Printf.sprintf "%.8g" x else 
                              Printf.sprintf "%.17g" x) "test5 32" 
      [-1.;-1.;-1.;-1.;-1.;-1.] @@ 
    test5
end

module M = Test2(CTrx)
module M = Test2(CC)
let _ = CC.print_code M.test5

(*
void fn(){
  bool x_43 = false;
  float x_44 = 0.;
  static float a_45[7] = {1.,2.,3.,4.,5.,6.,7.};;
  for (int i_46 = 0; i_46 < 7; i_46 += 1){
    float const t_47 = a_45[i_46];
    if (x_43)
    {
      float const t_48 = (-1. * t_47) + (1. * x_44);
      printf("%.8g\n",t_48);
      x_44 = t_47;
    }
    else {
      x_44 = t_47;
      x_43 = true;
    }
  }
}
*)

module TestG(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Window_fn.Make(C)(Raw)
  open Utils(C)

  let () = Printf.printf "\nGeneral case, %s backend\n" C.ident

  let test1 =
    let (module Win) = make_window C.tint 4 3 in
    iota C.(int 0) 
    |> Win.make_stream 
    |> Raw.map_raw (Win.dot C.tint C.int [|1;2;1;1|] C.( + ) C.( * )) 
    |> take C.(int 6)
    |> iter C.print_int

  let () = check_int_list "test1" [8;23;38;53;68;83] test1
  (*
   0 1 2 3 4 5 6 7 8 9 10 11  12 13 14  15 16 17 18
   1 1 2 1
         1 1 2 1
               1 2 1 1
                     1 2 1 1                     
                           1 2 1 1
  *)

  let test1p =
    let (module Win) = make_window C.tint 4 3 in
    iota C.(int 0) 
    |> Win.make_stream ~padding:true
    |> Raw.map_raw (Win.dot C.tint C.int [|1;2;1;1|] C.( + ) C.( * )) 
    |> take C.(int 7)
    |> iter C.print_int

  let () = check_int_list "test1p" [0;8;23;38;53;68;83] test1p

  let test2 =
    let (module Win) = make_window C.tint 11 2 in
    of_int_array [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15|] 
    |> Win.make_stream 
    |> Raw.map_raw (Win.reduce C.( + )) 
    |> iter C.print_int

  let () = check_int_list "test2" [66;88;110] test2

  (*
   01 02 03 04 05 06 07 08 09 10 11 12 13 14 15
    3  3 63 63 63 63 63 63 63 63 63 (= 66)
           7  7 81 81 81 81 81 81 81 81 81 (= 88)
                11 11 99 99 99 99 99 99 99 99 99 (= 110)
   *)

  let test2p =
    let (module Win) = make_window C.tint 11 2 in
    of_int_array [|1;2;3;4;5;6;7;8;9;10;11;12;13;14;15|] 
    |> Win.make_stream ~padding:true
    |> Raw.map_raw (Win.reduce C.( + )) 
    |> iter C.print_int

  let () = check_int_list "test2p" [1;6;15;28;45;66;88;110] test2p
end
module M = Test2(CTrx)
module M = TestG(CC)
let _ = CC.print_code M.test1



let () = print_endline "All done"
