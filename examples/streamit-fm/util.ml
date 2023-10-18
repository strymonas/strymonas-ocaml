let reduce : ('a -> 'a -> 'a) -> 'a list -> 'a = fun f -> function
  | h::t -> List.fold_left f h t
  | _    -> failwith "reduce: empty list"

let drop_last : 'a list -> 'a list =  fun l ->
  List.filteri (fun i _ -> i < List.length l - 1) l

(* compare two lists, which are should be essnetially equal *)
let cnter = ref 0
let check_identity_list ?name ?(threshold_ave_rel_err=1e-12) ?(threshold_max_rel_err=1e-6)
                        (temp_list: float list) (real_list: float list) 
                        : unit =
  let threshold_for_rel_err = 0.001 in
  Printf.printf "=== Check %s ===\n" (match name with Some x -> x | None -> incr cnter; string_of_int !cnter);
  let abs_err_list = 
    List.map2 (fun ti ri -> Float.abs (ti -. ri)) temp_list real_list in
  let rel_err_list =  
    List.combine abs_err_list real_list
    |> List.filter (fun (abs_err, ri) -> Float.abs ri >= threshold_for_rel_err)
    |> List.map (fun (abs_err, ri) -> abs_err /. Float.abs ri)
  in
  let ave_abs_err = 
    let n = List.length real_list in
    if n>0 then 
      List.fold_left ( +. ) 0. abs_err_list /. float n
    else
      0.
  in
  let ave_rel_err = 
    let n = List.length rel_err_list in
    if n>0 then
      List.fold_left ( +. ) 0. rel_err_list /. float n
    else
      0.
  in
  let (_, max_abs_err, i_max, actual, expected) =
    let mymax (cnt, max_ae, i, a, e) x y = 
      let xy = Float.abs (x -. y) in
      if xy > max_ae then
        (cnt+1,     xy, cnt, x, y)
      else
        (cnt+1, max_ae,   i, a, e)
    in
    List.fold_left2 mymax (0, 0., 0, 0., 0.) temp_list real_list
  in
  let (_, max_rel_err, i_max2, actual2, expected2) =
    let mymax (cnt, max_re, i, a, e) x y = 
      if Float.abs y >= threshold_for_rel_err then
        let xy_y = Float.abs (x -. y) /. Float.abs y in
        if xy_y > max_re then
          (cnt+1,   xy_y, cnt, x, y)
        else
          (cnt+1, max_re,   i, a, e)
      else
        (cnt+1, max_re,   i, a, e)
    in
    List.fold_left2 mymax (0, 0., 0, 0., 0.) temp_list real_list
  in
  Printf.printf "ave_abs_err: %.20f\n" ave_abs_err;
  Printf.printf "ave_rel_err: %.20f\n" ave_rel_err;
  Printf.printf "max abs err: %g = %g - %g, where i = %d\n" max_abs_err actual expected i_max;
  Printf.printf "max rel err: %g\n" max_rel_err;
  assert (ave_rel_err < threshold_ave_rel_err);
  assert (max_rel_err < threshold_max_rel_err)
