open! Core
open Z
open Z.Compare

let z = Z.of_int
let two = z 2
let three = z 3
let four = z 4
let five = z 5
let six = z 6
let seven = z 7
let eight = z 8
let nine = z 9

module BinPow = struct
  let non_bin_pow_rec_tco (base : Z.t) (exp : Z.t) : Z.t =
    let rec aux ?(acc = one) (base : Z.t) (exp : Z.t) : Z.t =
      match exp with
      | exp when exp = zero -> acc
      | exp -> aux ~acc:(base * acc) base (Z.pred exp)
    in
    aux base exp
  ;;

  let rec bin_pow_rec (base : Z.t) (exp : Z.t) : Z.t =
    match exp with
    | exp when exp = zero -> one
    | exp when exp = one -> base
    | exp ->
      let res = bin_pow_rec base (exp / two) in
      (match exp land one with
       | exp when Z.equal exp one -> res * res * base
       | _ -> res * res)
  ;;

  let bin_pow_rec_tco (base : Z.t) (exp : Z.t) : Z.t =
    let rec aux ?(acc = one) (base : Z.t) (exp : Z.t) : Z.t =
      let open Z.Compare in
      match exp with
      | exp when exp = zero -> one
      | exp when exp = one -> base * acc
      | exp ->
        (match exp land one with
         | rem when rem = one -> aux ~acc:(acc * base) (base * base) (exp / two)
         | _ -> aux ~acc (base * base) (exp / two))
    in
    aux base exp
  ;;

  let bin_pow_loop base exp =
    let res = ref one in
    let exp_ref = ref exp in
    let base_ref = ref base in
    while !exp_ref > zero do
      if !exp_ref land one = one then res := !res * !base_ref;
      base_ref := !base_ref * !base_ref;
      exp_ref := !exp_ref / two
    done;
    !res
  ;;
end

module BinPowBench = struct
  open Core_bench
  open BinPow

  let bin_pow_test_suite =
    [ "Bin exp pow", bin_pow_rec
    ; "Non bin exp pow", non_bin_pow_rec_tco
    ; "Bin pow rec TCO", bin_pow_rec_tco
    ; "Bin pow imperative", bin_pow_loop
    ]
  ;;

  let iters = [ 10; 100; 1000; 10000; 100000 ]

  let command =
    Bench.make_command
      (iters
       |> List.map ~f:(fun iter ->
         bin_pow_test_suite
         |> List.map ~f:(fun (name, func) ->
           Bench.Test.create ~name:[%string "%{name}-%{iter#Int}"] (fun () ->
             ignore (func two (z iter)))))
       |> List.fold ~init:[] ~f:(fun acc elem -> List.append elem acc))
  ;;
end

module BinPowTests = struct
  open BinPow

  let%test "non_bin_pow_rec_tco 0" = non_bin_pow_rec_tco two zero = one
  let%test "non_bin_pow_rec_tco 1" = non_bin_pow_rec_tco two one = two
  let%test "non_bin_pow_rec_tco 2" = non_bin_pow_rec_tco two two = four
  let%test "non_bin_pow_rec_tco 3" = non_bin_pow_rec_tco two three = eight
  let%test "bin_pow_rec 0" = bin_pow_rec two zero = one
  let%test "bin_pow_rec 1" = bin_pow_rec two one = two
  let%test "bin_pow_rec 2" = bin_pow_rec two two = four
  let%test "bin_pow_rec 3" = bin_pow_rec two three = eight
  let%test "bin_pow_rec_tco 0" = bin_pow_rec_tco two zero = one
  let%test "bin_pow_rec_tco 1" = bin_pow_rec_tco two one = two
  let%test "bin_pow_rec_tco 2" = bin_pow_rec_tco two two = four
  let%test "bin_pow_rec_tco 3" = bin_pow_rec_tco two three = eight
  let%test "bin_pow_loop 0" = bin_pow_loop two zero = one
  let%test "bin_pow_loop 1" = bin_pow_loop two one = two
  let%test "bin_pow_loop 2" = bin_pow_loop two two = four
  let%test "bin_pow_loop 3" = bin_pow_loop two three = eight
end
