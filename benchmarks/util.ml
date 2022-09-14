(*
open Oml.Statistics.Distributions
open Oml.Statistics.Descriptive
*)


let mean : float array -> float = fun arr ->
  (Array.fold_left (+.) 0.0 arr) /. (float @@ Array.length arr)


let var : float array -> float = fun arr ->
  let m = mean arr in
  (Array.fold_left (fun acc x ->
    let x' = x -. m in acc +. x'*.x') 0.0 arr) /. (float @@ Array.length arr)


let unbiased_var : float array -> float = fun arr -> 
  ((float @@ Array.length arr) *. var arr) /. (sqrt @@ float @@ Array.length arr - 1)


(* approximation under N ~ infinity *)
let standard_deviation : float array -> float = fun arr -> sqrt(var arr)


let t_distribution_table deg_free = 
  try
    List.assoc deg_free [
      ( 1, 12.706); 
      ( 2,  4.303); 
      ( 3,  3.182); 
      ( 4,  2.776); 
      ( 5,  2.571); 
      ( 6,  2.447); 
      ( 7,  2.365); 
      ( 8,  2.306); 
      ( 9,  2.262); 
      (10,  2.228); 
      (11,  2.201); 
      (12,  2.179); 
      (13,  2.160); 
      (14,  2.145); 
      (15,  2.131); 
      (16,  2.120); 
      (17,  2.110); 
      (18,  2.101); 
      (19,  2.093); 
      (20,  2.086); 
      (21,  2.080); 
      (22,  2.074); 
      (23,  2.069); 
      (24,  2.064); 
      (25,  2.060); 
      (26,  2.056); 
      (27,  2.052); 
      (28,  2.048); 
      (29,  2.045); 
      (30,  2.042)
    ]
  with Not_found ->
    failwith "Not supported sample size"


let mean_error confidence arr =
  let sample_size = Array.length arr in
  (* `a_2` means `a/2` where `a` is a significance level. *)
  (* `t` is the upper `a_2` point in t-distribution *)
  (* let a_2 = (1.0 -. confidence) /. 2.0 in
  let t = (student_quantile ~degrees_of_freedom:(float sample_size -. 1.0)
             (1.0 -. a_2)) in *)
  (* a value for 99.5% CI where a sample size is 30 (i.e. 29 degrees of freedom) *)
  (* let t = 3.038 in *)
  (* a value for 95%   CI where a sample size is 30 (i.e. 29 degrees of freedom) *)
  (* let t = 2.045 in *)
  let t = t_distribution_table (sample_size - 1) in
  t *. (sqrt @@ unbiased_var arr) /. (sqrt @@ float sample_size)


(* from https://github.com/ocaml/ocaml/blob/trunk/stdlib/seq.ml *)
open Seq
let rec take_aux n xs =
if n = 0 then
  empty
else
  fun () ->
    match xs() with
    | Nil ->
        Nil
    | Cons (x, xs) ->
        Cons (x, take_aux (n-1) xs)

let take n xs =
  if n < 0 then invalid_arg "Seq.take";
  take_aux n xs

let rec map2 f xs ys () =
  match xs() with
  | Nil ->
      Nil
  | Cons (x, xs) ->
      match ys() with
      | Nil ->
          Nil
      | Cons (y, ys) ->
          Cons (f x y, map2 f xs ys)