open! Core

module BinPow = struct
  open Z
  open Z.Compare

  let rec non_bin_pow_rec_tco ?(acc = Z.of_int 1) (base : Z.t) (exp : Z.t) : Z.t =
    match exp with
    | exp when exp = Z.of_int 0 -> acc
    | exp -> non_bin_pow_rec_tco ~acc:(base * acc) base (Z.pred exp)
  ;;

  let%test "non_bin_pow_rec_tco 0" =
    non_bin_pow_rec_tco (Z.of_int 2) (Z.of_int 0) = Z.of_int 1
  ;;

  let%test "non_bin_pow_rec_tco 1" =
    non_bin_pow_rec_tco (Z.of_int 2) (Z.of_int 1) = Z.of_int 2
  ;;

  let%test "non_bin_pow_rec_tco 2" =
    non_bin_pow_rec_tco (Z.of_int 2) (Z.of_int 2) = Z.of_int 4
  ;;

  let%test "non_bin_pow_rec_tco 3" =
    non_bin_pow_rec_tco (Z.of_int 2) (Z.of_int 3) = Z.of_int 8
  ;;

  let rec bin_pow_rec (base : Z.t) (exp : Z.t) : Z.t =
    let open Z in
    match exp with
    | exp when exp = Z.of_int 0 -> Z.of_int 1
    | exp when exp = Z.of_int 1 -> base
    | exp ->
      let res = bin_pow_rec base (exp / Z.of_int 2) in
      (match exp land Z.of_int 1 with
       | exp when Z.equal exp (Z.of_int 1) -> res * res * base
       | _ -> res * res)
  ;;

  let%test "bin_pow_rec 0" = bin_pow_rec (Z.of_int 2) (Z.of_int 0) = Z.of_int 1
  let%test "bin_pow_rec 1" = bin_pow_rec (Z.of_int 2) (Z.of_int 1) = Z.of_int 2
  let%test "bin_pow_rec 2" = bin_pow_rec (Z.of_int 2) (Z.of_int 2) = Z.of_int 4
  let%test "bin_pow_rec 3" = bin_pow_rec (Z.of_int 2) (Z.of_int 3) = Z.of_int 8

  let rec bin_pow_rec_tco ?(acc = Z.of_int 1) (base : Z.t) (exp : Z.t) : Z.t =
    let open Z.Compare in
    match exp with
    | exp when exp = Z.of_int 0 -> Z.of_int 1
    | exp when exp = Z.of_int 1 -> base * acc
    | exp ->
      (match exp land Z.of_int 1 with
       | rem when rem = Z.of_int 1 ->
         bin_pow_rec_tco ~acc:(acc * base) (base * base) (exp / Z.of_int 2)
       | _ -> bin_pow_rec_tco ~acc (base * base) (exp / Z.of_int 2))
  ;;

  let%test "bin_pow_rec_tco 0" = bin_pow_rec_tco (Z.of_int 2) (Z.of_int 0) = Z.of_int 1
  let%test "bin_pow_rec_tco 1" = bin_pow_rec_tco (Z.of_int 2) (Z.of_int 1) = Z.of_int 2
  let%test "bin_pow_rec_tco 2" = bin_pow_rec_tco (Z.of_int 2) (Z.of_int 2) = Z.of_int 4
  let%test "bin_pow_rec_tco 3" = bin_pow_rec_tco (Z.of_int 2) (Z.of_int 3) = Z.of_int 8

  let bin_pow_loop base exp =
    let res = ref (Z.of_int 1) in
    let exp_ref = ref exp in
    let base_ref = ref base in
    while !exp_ref > Z.of_int 0 do
      if !exp_ref land Z.of_int 1 = Z.of_int 1 then res := !res * !base_ref;
      base_ref := !base_ref * !base_ref;
      exp_ref := !exp_ref / Z.of_int 2
    done;
    !res
  ;;

  let%test "bin_pow_loop 0" = bin_pow_loop (Z.of_int 2) (Z.of_int 0) = Z.of_int 1
  let%test "bin_pow_loop 1" = bin_pow_loop (Z.of_int 2) (Z.of_int 1) = Z.of_int 2
  let%test "bin_pow_loop 2" = bin_pow_loop (Z.of_int 2) (Z.of_int 2) = Z.of_int 4
  let%test "bin_pow_loop 3" = bin_pow_loop (Z.of_int 2) (Z.of_int 3) = Z.of_int 8
end
