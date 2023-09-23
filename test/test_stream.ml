(* Regression tests *)

(*
#directory "../lib";;
#directory "../lib/backends/Trx";;
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


(* First, some smaller tests, also to check the generated code *)

module CodeBasic = struct
  include Trx_code
  let ident = "MetaOCaml, Basic"
end

module CodePV = struct
  include Pk_cde.Make(Trx_code)
end

module CCodePV = struct
  include Pk_cde.Make(C_cde)
end

module MiscTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nMisc Test, %s backend\n" C.ident

  let test_sum = from_to C.(int 1) C.(int 10) |> sum_int
  let () = check_int "test_sum" 55 @@ test_sum

  (* XXX
  let test_average = from_to C.(int 1) C.(int 10) |> average_int
  let () = check_float "test_average" 5.5 @@ test_average
  *)

  let test_scan1 = from_to C.(int 1) C.(int 10) 
      |> scan C.( + ) C.(int 0) |> sum_int
  let () = check_int "test_scan1" 
     (List.fold_left ( + ) 0 [1;3;6;10;15;21;28;36;45;55]) @@ test_scan1

  let test_atan64 = 
    from_to C.(int 1) C.(int 10) |> map C.F64.of_int |>
    map C.F64.atan.invoke |> 
    fold_ Desc.Single C.F64.(+.) C.(F64.lit 0.) C.F64.print
  let () = check_list reader_single (Printf.sprintf "%.17g") "test_atan 64" 
      (List.init 10 succ |> List.map float_of_int |>
      List.map atan |> List.fold_left ( +. ) 0. |> fun x -> [x]) @@ 
    test_atan64

  let test_atan32 = 
    from_to C.(int 1) C.(int 10) |> map C.F32.of_int |>
    map C.F32.atan.invoke |> 
    fold_ Desc.Single C.F32.(+.) C.(F32.lit 0.) C.F32.print
  let () = check_list reader_single 
      (fun x -> 
        if C.ident = "C" then Printf.sprintf "%.8g" x else 
                              Printf.sprintf "%.17g" x) "test_atan 32" 
      (List.init 10 succ |> List.map float_of_int |>
      List.map atan |> List.fold_left ( +. ) 0. |> fun x -> [x]) @@ 
    test_atan32

  let test_complex1 = 
    let open C in
    let open C.C32 in
    letl (lit Complex.{re=2.;im=3.}) @@ fun a ->
    letl (lit Complex.{re=4.;im=5.}) @@ fun b ->
    new_uarray C32.tbase 10 @@ fun arr ->
    array_set arr (int 0) (conj (a +. b)) @.
    array_get arr (int 0) @@ fun v ->
    letl (v *. a) @@ fun v ->
    complex (imag v) (real v) |> ret

  (* let () = C.print_code test_complex1 *)

  let string_of_cmlx x = let Complex.{re;im} = C.C32.of_t x in
  Printf.sprintf "%.1f%+.1f*I" re im

  let () = check "test complex1" string_of_cmlx
      (C.C32.to_t Complex.{re= 2.;im=36.})
      test_complex1 

  let test_complex2 = 
    let open C in
    let open C.C32 in
    of_static_arr C32.tbase (fun i -> lit {Complex.re=i; im=1.}) 
      (List.init 5 succ |> List.map float_of_int |> Array.of_list) |>
    map norm2 |>
    fold F32.(+.) F32.(lit 0.)

  (* let () = C.print_code test_complex2 *)

  let () = check "test complex2" (fun x -> C.F32.of_t x |> string_of_float)
      (List.init 5 succ |> List.map float_of_int |> 
      List.map (fun x -> Complex.{re=x; im=1.}) |>
      List.map Complex.norm2 |>
      List.fold_left (+.) 0. |> C.F32.to_t)
      test_complex2
end

module M = MiscTest(CodePV)
module M = MiscTest(CCodePV)

module SimpleTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nSimpleTest, %s backend\n" C.ident

  let test1 =
    of_int_array [|1;2;3|]
     |> fold C.(+) C.(int 0)

  let () = check_int "fold_test" 6 test1

  let test2 =
    of_float_array [|1.0;2.0;3.0|]
     |> map C.F64.truncate
     |> sum_int

  let () = check_int "map_fold_test" 6 test2

  let test3 =
    of_float_array [|1.0;2.0;3.0;4.0|]
    |> map C.F64.truncate
    |> filter C.(fun x -> x mod int 2 = int 0)
    |> map C.(fun x -> x * x)
    |> sum_int

  let () = check_int "map_filter_map_fold_test" 20 test3

  let test31 =                            (* demo/poster *)
    iota (C.int 1)
    |> map C.(fun x -> x * x)
    |> take C.(int 10)
    |> sum_int

  let test31_ex = let r  = ref 0 in for i=1 to 10 do r:= !r + i*i done; !r

  let () = check_int "test31" 385 test31

  let test32 =                            (* JSSST paper *)
    iota (C.int 1)
    |> map C.(fun e -> int 2 * e)
    |> take (C.int 10)
    |> sum_int

  let () = check_int "test32" 110 test32

  let test4 =
    of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0;7.0|]
    |> map C.F64.truncate
    |> filter C.(fun x -> x mod int 2 = int 0)
    |> take (C.int 2)
    |> map C.(fun x -> x * x)
    |> sum_int

  let () = check_int "map_filter_map_fold_test" 20 test4

  let () = check_int_list "enum: testenum" [1; 2; 3; 4; 5] @@ begin
    from_to (C.int 1) (C.int 5)
    |> iter C.print_int
  end


  let () = check_int "test_for_map_by_fold" 220 @@ begin
  iota (C.int 1)
  |> map_accum C.(fun z e k -> letl (z+e) @@ fun v -> k v v) (C.int 0)
  |> take (C.int 10)
  |> sum_int
  end
  (*
  let rec f n =
    if n = 0 then 0
    else n + f (n-1)
  and g n e =
    if n > e then 0
    else
      f n + g (n+1) e
  (1)+(1+2)+(1+2+3)+...+(1+2+...+10) = g 1 10*)


  let () = check_int_list "take_fold" 
    [10; 11; 12; 13; 14] @@ begin
  iota (C.int 10)
  |> take (C.int 5)
  |> iter C.print_int
  end
end

(*
module M = SimpleTest(CodeBasic)
let _ = M.test1
let _ = M.test2
let _ = M.test31
let _ = M.test32
*)

module M = SimpleTest(CodePV)
let _ = M.test3
let _ = M.test31
let _ = M.test32

module M = SimpleTest(CCodePV)

(* MetaOCaml-specific *)
module SimpleTestOCaml = struct
  module C = CodeBasic
  open Stream_cooked_fn.Make(C)

  open Utils(C)

  let () = Printf.printf "\nMetaOCaml-specific test\n"

  let test3 =
    of_float_array [|1.0;2.0;3.0;4.0|]
    |> map C.F64.truncate
    |> filter C.(fun x -> x mod int 2 = int 0)
    |> map C.((make_ff Stdlib.(fun x -> .<.~x * .~x>.)).invoke)
    |> sum_int

  let () = check_int "map_filter_map_fold, OCaml" 20 test3

  let () = check "enum: testenum" (show_list string_of_int)
      (List.rev [1; 2; 3; 4; 5]) @@ begin
    from_to (C.int 1) (C.int 5)
    |> fold (fun z a -> C.cons a z) C.nil
  end

  let () = check "enum: testenum, OCaml" (show_list string_of_int)
      ([1; 2; 3; 4; 5]) @@ begin
    from_to (C.int 1) (C.int 5)
    |> fold (fun z a -> C.cons a z) C.nil
    |> C.((make_ff1 .<List.rev>.).invoke)
  end

  let () = check "take_fold" (show_list string_of_int)
    [14; 13; 12; 11; 10] @@ begin
  iota (C.int 10)
  |> take (C.int 5)
  |> fold (fun z x -> C.cons x z) C.nil
  end
end

module MainTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nMainTest, %s backend\n" C.ident

  let plus_arr arr = fun x -> of_int_array arr |> map C.(fun c -> x + c)
  let sum_from_to step a b = 
    if step >= 0
    then
      let rec loop a b =
        if a > b then 0 else (a + loop (a+step) b)
      in loop a b
    else 
      let rec loop a b =
        if a > b then 0 else (b + loop a (b+step))
      in loop a b
      

  let test50 =
    of_int_array [|0;1;2;3;4;5|]
    (*
    |> flat_map C.(fun x -> new_array [|x; x+int 1; x+int 2|] of_arr)
    *)
    |> flat_map (plus_arr [|0;1;2|])
    |> iter C.print_int

  let () = check_int_list "flatmap_fold" 
    [0; 1; 2; 1; 2; 3; 2; 3; 4; 3; 4; 5; 4; 5; 6; 5; 6; 7] test50

  let test5 =
    of_int_array [|0;1;2;3;4;5|]
    |> filter even
    |> flat_map (plus_arr [|0;1;2|])
    |> iter C.print_int

  let () = check_int_list "filter_flatmap_fold" 
    [0; 1; 2; 2; 3; 4; 4; 5; 6] test5

  let test7 =
    iota (C.int 1)
  |> flat_map (fun x -> iota x) (* Infinite inner stream *)
  |> filter even
  |> take (C.int 10)
  |> iter C.print_int

  let () = check_int_list "flatmap: test7" 
    [2; 4; 6; 8; 10; 12; 14; 16; 18; 20] test7

  let () = check_int_list "flatmap: test71" 
    [4; 6; 8; 10; 12; 14; 16; 18; 20; 22] @@ begin
  iota (C.int 1)
  |> flat_map (fun x -> iota C.(x+int 1)) (* Infinite inner stream *)
  |> flat_map (fun x -> iota C.(x+int 1)) (* Infinite inner stream *)
  |> flat_map (fun x -> iota C.(x+int 1)) (* Infinite inner stream *)
  |> filter even
  |> take (C.int 10)
  |> iter C.print_int
  end

  let () = check_int_list "flatmap: test72" 
    [6; 8; 10; 12; 14; 16; 18; 20; 22; 24] @@ begin
  iota (C.int 1)
  |> flat_map (fun x -> iota C.(x+int 1)) (* Infinite inner stream *)
  |> flat_map (fun x -> iota C.(x+int 1)) (* Infinite inner stream *)
  |> filter even
  |> flat_map (fun x -> iota C.(x+int 1)) (* Infinite inner stream *)
  |> filter even
  |> take (C.int 10)
  |> iter C.print_int
  end

  let () = check_int_list "flatmap: test8" 
      [2; 4; 4; 4; 6; 6; 6; 8; 8; 8] @@ begin
  iota (C.int 1)
  |> flat_map (fun x -> iota C.(x+int 1) |> take (C.int 3))
  |> filter even
  |> take (C.int 10)
  |> iter C.print_int
  end

  let () = check_int_list "flatmap: test8'" [2; 3] @@ begin
    iota (C.int 1)
    |> flat_map (fun x -> iota C.(x * int 1)
                          |> flat_map (fun x -> iota C.(x * int 2))
                          |> take (C.int 3) )
    |> take (C.int 2)
    |> iter C.print_int
    end

  let () = check_int_list "flatmap: test9"
    [114; 114; 216; 114; 216; 114; 216; 216; 216; 120] @@ begin
  iota (C.int 1)
  |> flat_map (fun x -> iota C.(x+int 10)
			|> flat_map (plus_arr [|100;200|])
			|> filter (fun x -> C.(x mod int 3 = int 0))
			|> take (C.int 3))
  |> filter even
  |> take (C.int 10)
  |> iter C.print_int
  end

  let () = check_int_list "minmax" [0;5] @@ begin
   of_int_array [|0;3;2;5;1|]
   |> fold_ Desc.Tuple 
       (fun (mn,mx) a -> (C.imin mn a, C.imax mx a)) (C.int 100, C.int (-1))
       C.(fun (mn,mx) -> seq (print_int mn) (print_int mx))
  end


  let () = check_int "test_fromto1" 55 @@ begin
    from_to (C.int 0) (C.int 10) |> sum_int
  end

  let () = check_int "test_fromto2" 52  @@ begin
    from_to (C.int 3) (C.int 10) |> sum_int
  end

  let () = check_int "test_fromto3" (sum_from_to 2 3 50) @@ begin
    from_to ~step:2 (C.int 3) (C.int 50) |> sum_int
  end

  let () = check_int "test_fromto4" (sum_from_to ~-2 3 50) @@ begin
    from_to ~step:(-2) (C.int 50) (C.int 3) |> sum_int
  end
end

module M = MainTest(CodePV)
let _ = M.test7

module M = MainTest(CCodePV)

module ZipTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nZipTest, %s backend\n" C.ident

  (* utilities *)
  let plus_arr arr = fun x -> of_int_array arr |> map C.(fun c -> x + c)

  let check_intfloat_list = check_list reader_tuple 
      (fun (x,y) -> Printf.sprintf "%d,%g" x y)

  let testz0 =
    zip_with C.( * )
      (of_int_array [|1;2;3;4;5;6|])
      (iota (C.int 1))
    |> sum_int

  let () = check_int "zip: testz0" 91 testz0

  let testz01 =                           (* from my notes; look at the code *)
    zip_with C.( * )
	   (of_int_array [|1;2;3;4;5;6|])
	   (of_int_array [|10; 20; 30; 40; 50|]
	      |> filter (fun x -> C.(x mod int 20 = int 0)))
    |> sum_int

  let () = check_int "zip: testz01" 100 testz01

  (* add zips when one stream is infinite; zips with two flattened maps;
     and perhaps one is infinite
   *)
  let testz1 =
  Raw.zip_raw
	    (
	      of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0|]
	      |> map C.F64.truncate
	      |> take (C.int 5)
	      |> filter even
	      |> map square)
	    (
	      iota (C.int 10)
	      |> map C.(fun x -> F64.of_int (x * x)))
  |> iter C.(fun (x,y) -> seq (print_int x) (F64.print y))

  let () = check_intfloat_list "zip: testz1"
    [(4, 100.); (16, 121.)] testz1

  (* switching the order of streams *)
  let () = check_intfloat_list "zip: testz1 swap"
      [(4, 100.); (16, 121.)] @@ begin
   Raw.zip_raw
        (
	      iota (C.int 10)
	      |> map C.(fun x -> F64.of_int (x * x)))
	    (
	      of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0|]
	      |> map C.F64.truncate
	      |> take (C.int 5)
	      |> filter even
	      |> map square)
  |> iter C.(fun (y,x) -> seq (print_int x) (F64.print y))
  end

  (* Filters and counters on both streams *)
  let () = check_intfloat_list "zip: testz2"
    [(4, 441.); (16, 576.); (36, 729.)] @@ begin
  Raw.zip_raw
	    (
	      of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0|]
	      |> map C.F64.truncate
	      |> take (C.int 10)
	      |> filter even
	      |> map square)
	    (
	      iota (C.int 20)
	      |> filter (fun x -> C.(x mod int 3 = int 0))
	      |> map C.(fun x -> F64.of_int (x * x))
	      |> take (C.int 3))
  |> iter C.(fun (x,y) -> seq (print_int x) (F64.print y))
  end

  let () = check_intfloat_list "zip: testz21"
    [(4, 441.); (16, 576.)] @@ begin
  Raw.zip_raw
	    (
	      of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0|]
	      |> map C.F64.truncate
	      |> filter even
	      |> take (C.int 2)
	      |> map square)
	    (
	      iota (C.int 20)
	      |> filter (fun x -> C.(x mod int 3 = int 0))
	      |> map C.(fun x -> F64.of_int (x * x))
	      |> take (C.int 3))
  |> iter C.(fun (x,y) -> seq (print_int x) (F64.print y))
  end

  let testz3 =
    Raw.zip_raw
	    (
	      of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0|]
	      |> take (C.int 5))
	    (
	      iota (C.int 1)
	      |> flat_map (fun x -> iota C.(x+int 1) |> take (C.int 3)))
  |> iter C.(fun (y,x) -> seq (print_int x) (F64.print y))

  let () = check_intfloat_list "zip: testz3"
    [(2, 1.); (3, 2.); (4, 3.); (3, 4.); (4, 5.)] testz3

  let () = check_intfloat_list "zip: testz3 swapped"
      [(2, 1.); (3, 2.); (4, 3.); (3, 4.); (4, 5.)] @@ begin
    Raw.zip_raw
	    (
	      iota (C.int 1)
	      |> flat_map (fun x -> iota C.(x+int 1) |> take (C.int 3)))
	    (
	      of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0|]
	      |> take (C.int 5))
  |> iter C.(fun (x,y) -> seq (print_int x) (F64.print y))
  end

  let testz4 =
      Raw.zip_raw
	 (
	  of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0;9.0;10.0|]
          |> map C.F64.truncate
	  |> take (C.int 12)
	  |> filter even
	  |> map square)
	 (
	  iota (C.int 1)
          |> flat_map (fun x -> iota C.(x+int 1) |> take (C.int 3))
	  |> filter even)
  |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testz4"
    [(4, 2); (16, 4); (36, 4); (64, 4); (100, 6)] @@ testz4

  let () = check_intint_list "zip: testz4'"
   [(4, 2); (16, 3); (36, 4); (64, 3); (100, 4); (144, 5); 
              (196, 4); (256, 5); (324, 6); (400, 5)] @@ begin
  Raw.zip_raw
	    (
	      iota (C.int 1)
	      |> take (C.int 20)
	      |> filter even
	      |> map square)
	    (
	      iota (C.int 1)
	      |> flat_map (fun x -> iota C.(x+int 1) |> take (C.int 3)))
  |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))
  end

  (* Example from the paper *)
  let test_paper = Raw.zip_raw
      (* First stream to zip *)
      (of_int_array [|0;1;2;3|]
        |> map square
        |> take (C.int 12)
        |> filter even
        |> map square)
      (* Second stream to zip *)
      (iota (C.int 1)
        |> flat_map (fun x -> iota C.(x+int 1) |> take (C.int 3))
        |> filter even)
      |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "from the paper: test_paper"
     [(0, 2); (16, 4)] test_paper

  let testz9 =
    Raw.zip_raw
      C.(from_to (int 0) (int 56) |> filter C.(fun e -> (e mod int 3) = int 2))
      C.(from_to (int 1) (int 13) |> filter C.(fun e -> (e mod int 3) = int 2))
    |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testz9"
    [(2, 2); (5, 5); (8, 8); (11, 11)] @@ testz9
end

(*
module M = ZipTest(CodeBasic)
let _ = M.testz0
let _ = M.testz01
let _ = M.testz4
let _ = M.test_paper
let c = M.testz9
*)

module M = ZipTest(CodePV)
(*
let _ = M.testz0
let _ = M.testz01
let _ = M.test_paper
let _ = M.testz9
*)

module M = ZipTest(CCodePV)

module ZipDeepTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nZipDeep Test, %s backend\n" C.ident

  let plus_arr arr = fun x -> of_int_array arr |> map C.(fun c -> x + c)

  let check_intfloat_list = check_list reader_tuple 
      (fun (x,y) -> Printf.sprintf "%d,%g" x y)

  (* Nested zips *)
  let testz5 =
    Raw.zip_raw
      (of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0|] |> take (C.int 5))
      (Raw.zip_raw
	(of_float_array [|1.0;2.0;3.0;4.0;5.0;6.0|] |> take (C.int 12))
	(iota (C.int 1)
            |> flat_map (fun x -> iota C.(x+int 1) |> take (C.int 3))))
    |> iter C.(fun (x,(y,z)) -> 
        seq (F64.print x) @@ seq (F64.print y) (print_int z))

  let () = check_list 
      (fun c -> Scanf.bscanf c "%s\n%s\n%s\n" 
          (fun x y z -> x ^ "," ^ y ^ "," ^ z))
      (fun (x,(y,z)) -> Printf.sprintf "%g,%g,%d" x y z)
    "zip: testz5"
    [(1., (1., 2)); (2., (2., 3)); (3., (3., 4)); (4., (4., 3)); (5., (5., 4))]
    testz5

  let () = check_int "zip: testz6" 10 @@ begin
    zip_with C.(+)
        (zip_with C.( * )
          (of_int_array [|0;1;2;3;4|] |> map C.(fun a -> a * (int 1)))
          (of_int_array [|0;1;2;3|]))
        (zip_with C.( / )
          (of_int_array [|0;1;2;3;4|] |> map C.(fun a -> a * (int 2)))
          (of_int_array [|1;2;3|]))
       |> map C.(fun a -> int 1 + a)
       |> sum_int
  end

  let testz70 = Raw.zip_raw
   (from_to (C.int 1) (C.int 10) |> filter even)
   (iota (C.int 1) |> filter C.(fun x -> x mod (int 3) = int 0))
    |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testz70"
    [(2, 3); (4, 6); (6, 9); (8, 12); (10, 15)] @@ testz70

  let () = check_int "zip: testz7" 10 @@ begin
    zip_with C.( + )
        (zip_with C.( * )
          (of_int_array [|-1;0;-1;1;-1;2;3;4|]
           |> filter C.(fun a -> a >= int 0))
          (of_int_array [|0;1;10;2;3|]
           |> filter C.(fun a -> a < int 10)))
        (zip_with C.( / )
          (of_int_array [|-1;-1;-1;0;1;-1;-1;2;3;4|]
           |> map C.(fun a -> a * int 2)
           |> filter C.(fun a -> a >= int 0))
          (of_int_array [|1;2;3|]))
       |> map C.(fun a -> int 1 + a)
       |> sum_int
  end

  (* The most complex example: zipping of deeply nested streams *)
  let testxx =
    of_int_array [|0;1;2;3|]
       |> flat_map (plus_arr [|0; 1|])
       |> flat_map (plus_arr [|0; 1|])
       |> filter even
       |> iter C.print_int

  let () = check_int_list "zip: testxx"
     [0; 2; 2; 2; 2; 4; 4; 4]
   testxx

  let testyy =
    of_int_array [|1;2;3|]
       |> flat_map (plus_arr [|0; 1|])
       |> filter even
       |> flat_map (plus_arr [|0; 1|])
       |> iter C.print_int

  let () = check_int_list "zip: testyy"
     [2; 3; 2; 3; 4; 5] @@ testyy

  (* zipping of two flat-mapped streams *)
  let testzff1 = Raw.zip_raw
      (of_int_array [|10;20;30|] |>
         flat_map (fun e -> iota e |> take (C.int 5)))
      (from_to ~step:10 (C.int 10) (C.int 40) |>
         flat_map (fun e -> iota C.(int 100 + e) |> take (C.int 3)))
       |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testzff1"
  [(10, 110); (11, 111); (12, 112); (13, 120); (14, 121); 
             (20, 122); (21, 130); (22, 131); (23, 132); (24, 140); 
             (30, 141); (31, 142)] @@ testzff1

  let () = check_intint_list "zip: testzff1"
      [(10, 110); (11, 111); (12, 112); (13, 120); (14, 121); 
       (20, 122); (21, 130); (22, 131); (23, 132); (24, 140); 
       (30, 141); (31, 142)] @@ begin
     Raw.zip_raw
      (of_int_array [|10;20;30|] |>
         flat_map (fun e -> iota e |> take (C.int 5)))
      (of_int_array [|10;20;30;40|] |>
         flat_map (fun e -> iota C.(int 100 + e) |> take (C.int 3)))
       |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))
    end

  let () = check_intint_list "zip: testzff1"
      [(10, 110); (11, 111); (12, 112); (13, 120); (14, 121); 
             (20, 122); (21, 130); (22, 131); (23, 132); (24, 140); 
             (30, 141); (31, 142)] @@ begin
     Raw.zip_raw
      (of_int_array [|10;20;30|] |>
         flat_map (fun e -> iota e |> take (C.int 5)))
      (of_int_array [|10;20;30;40|] |>
         flat_map (plus_arr [|100;101;102|]))
       |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))
    end

  let testzff2 = Raw.zip_raw
      (of_int_array [|10;20;30|] |>
         flat_map (fun e -> iota e |> take (C.int 5)))
      (from_to ~step:10 (C.int 10) (C.int 40) |>
         flat_map (fun e -> iota C.(int 100 + e) |> take (C.int 3)))
      |> take (C.int 7)
      |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testzff2"
      [(10, 110); (11, 111); (12, 112); (13, 120); (14, 121); 
       (20, 122); (21, 130)] @@ testzff2

  let testzff3 = Raw.zip_raw
      (of_int_array [|10;11;20;21;30;31|] |>
         filter even |>
         flat_map (fun e -> iota e |> take (C.int 5)))
      (from_to ~step:10 (C.int 10) (C.int 40) |>
         flat_map (fun e -> iota C.(int 100 + e) |> take (C.int 3)))
      |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testzff3"
      [(10, 110); (11, 111); (12, 112); (13, 120); (14, 121); 
       (20, 122); (21, 130); (22, 131); (23, 132); (24, 140); 
       (30, 141); (31, 142)] @@ testzff3

  let () = check_intint_list "zip: testzff4"
    [(12, 110); (21, 112); (24, 120); (30, 122); (33, 130)] @@ 
    begin
    Raw.zip_raw
      (of_int_array [|10;11;20;21;30;31|] |>
         filter even |>
         flat_map (fun e -> iota e |> take (C.int 5) |>
                            filter C.(fun x -> x mod (int 3) = int 0)))
      (from_to ~step:10 (C.int 10) (C.int 40) |>
         flat_map (fun e -> iota C.(int 100 + e) |> take (C.int 3) |>
                            filter even))
      |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))
    end

  let testzff5 = Raw.zip_raw
      (of_int_array [|0;1;2;3|]
       |> flat_map (plus_arr [|0;1|])
       |> flat_map (plus_arr [|0;1|])
       |> filter even)
      (from_to ~step:10 (C.int 10) (C.int 40) |>
         flat_map (fun e -> iota C.(int 100 + e) |> take (C.int 3) |>
                            filter even))
       |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testzff5"
     [(0, 110); (2, 112); (2, 120); (2, 122); (2, 130); 
      (4, 132); (4, 140); (4, 142)] @@ testzff5

  let testz8 = Raw.zip_raw
      (of_int_array [|0;1;2;3|]
       |> flat_map (plus_arr [|0;1|])
       |> flat_map (plus_arr [|0;1|])
       |> filter even)
      (of_int_array [|1;2;3|]
       |> flat_map (plus_arr [|0;1|])
       |> filter even
       |> flat_map (plus_arr [|0;1|]))
     |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testz8"
     [(0, 2); (2, 3); (2, 2); (2, 3); (2, 4); (4, 5)] @@ testz8

  let testz81 = 
    let thisandnext e = 
      from_to (C.int 0) (C.int 1) |> map C.(fun i -> i + e) in 
    Raw.zip_raw
      (of_int_array [|0;1;2;3|]
       |> flat_map thisandnext
       |> flat_map thisandnext
       |> filter even)
      (of_int_array [|1;2;3|]
       |> flat_map thisandnext
       |> filter even
       |> flat_map thisandnext)
     |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  let () = check_intint_list "zip: testz81"
     [(0, 2); (2, 3); (2, 2); (2, 3); (2, 4); (4, 5)] @@ testz81
end

module M = ZipDeepTest(CodePV)
(*
let _ = M.testz70
let _ = M.testxx
let _ = M.testz8
let _ = M.testz81
*)

module M = ZipDeepTest(CCodePV)

(* ========== EXTRA FUNCTIONS ========== *)
module DropTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nDrop Test, %s backend\n" C.ident

  let test_drop1 = from_to (C.int 1) (C.int 10) |> drop (C.int 10) |> sum_int
  let test_drop2 = from_to (C.int 1) (C.int 10) |> drop (C.int  0) |> sum_int
  let test_drop3 = from_to (C.int 1) (C.int 10) |> drop (C.int  3) |> sum_int

  let () = check_int "test_drop1"  0 test_drop1
  let () = check_int "test_drop2" 55 test_drop2
  let () = check_int "test_drop3" 49 test_drop3

  let test_drop4 = 
    from_to (C.int 1) (C.int 48)
    |> filter C.(fun x -> x mod int 2 = int 0)
    |> drop (C.int 3)
    |> filter C.(fun x -> x mod int 3 = int 0)
    |> drop (C.int 1)
    |> take (C.int 5)
    |> iter C.print_int

  let () = check_int_list "test_drop4" [18; 24; 30; 36; 42] test_drop4

  let test_drop5 =
    Raw.zip_raw
      (iota (C.int 1) |> drop (C.int 3))
      (iota (C.int 1) |> drop (C.int 5))
    |> take (C.int 5) 
    |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  (*
  4 5 6 7 8
  6 7 8 9 10
  let [(8,10); (7,9); (6,8); (5,7); (4,6)] = Runcode.run test_drop5
  *)

  let () = check_intint_list "test_drop5" 
      [(4,6); (5,7); (6,8); (7,9); (8,10)] @@ test_drop5

  let test_drop6 =
    Raw.zip_raw
      (iota (C.int 1) |>
       filter C.(fun x -> x mod int 2 = int 0) |> 
       drop (C.int 3))
      (iota (C.int 1) |>
       filter C.(fun x -> x mod int 3 = int 0) |> 
       drop (C.int 5))
    |> take (C.int 5) 
    |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))

  (*
  2 4 6 8 10 12 ...
  8 10 12 ...

  3 6 9 12 15 18 21 24 ...
  18 21 24 ...
  *)

  let () = check_intint_list "test_drop6"
      [(8,18); (10,21); (12,24); (14,27); (16,30)] test_drop6

  (* drop and flat_map *)
end


module M = DropTest(CodePV)

module M = DropTest(CCodePV)

module DropWhileTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nDropWhile Test, %s backend\n" C.ident

  let plus_arr arr = fun x -> of_int_array arr |> map C.(fun c -> x + c)

  let test_drop_while1 =
    of_int_array [|1;3;4;5;6;3;2;1;10|]
    |> drop_while C.(fun e -> e < int 5)
    |> sum_int
    
  let () = check_int "test_drop_while1" (5+6+3+2+1+10) @@ test_drop_while1
  
  let test_drop_while2 =
    Raw.zip_raw
      (iota (C.int 1) |> drop_while C.(fun e -> e < int 3))
      (iota (C.int 1) |> drop_while C.(fun e -> e < int 5))
    |> take (C.int 5) 
    |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))
  
  (*
  3 4 5 6 ...
  5 6 7 8 ...
  *)
  
  let () = check_intint_list "test_drop_while2" 
      [(3,5); (4,6); (5,7); (6,8); (7,9)] @@ test_drop_while2
  
  (* drop_while and filter, flat_map, zip *)  
end

module M = DropWhileTest(CodePV)
module M = DropWhileTest(CCodePV)

module TakeWhileTest(C: cde) = struct
  open Stream_cooked_fn.Make(C)
  open Utils(C)

  let () = Printf.printf "\nTakeWhile Test, %s backend\n" C.ident

  let plus_arr arr = fun x -> of_int_array arr |> map C.(fun c -> x + c)

  let test_take_while1 =
    of_int_array [|1;3;4;5;6;3;2;1;10|]
    |> take_while C.(fun e -> e < int 5)
    |> sum_int
    
  let () = check_int "test_take_while1" (1+3+4) @@ test_take_while1
  
  let test_take_while2 =
    Raw.zip_raw
      (iota (C.int 1) |> take_while C.(fun e -> e < int 3))
      (iota (C.int 1) |> take_while C.(fun e -> e < int 5))
    |> take (C.int 5) 
    |> iter C.(fun (x,y) -> seq (print_int x) (print_int y))
  
  (*
  1 2
  1 2 3 4
  *)
  
  let () = check_intint_list "test_take_while2" 
      [(1,1); (2,2)] @@ test_take_while2
  
  (* take_while and filter, flat_map, zip *)  
end

module M = TakeWhileTest(CodePV)
module M = TakeWhileTest(CCodePV)



let () = print_endline "All done"
