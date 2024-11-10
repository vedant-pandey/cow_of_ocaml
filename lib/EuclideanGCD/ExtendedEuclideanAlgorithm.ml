open! Core

module ExtendedEuclideanAlgorithm = struct
  open! Z
  open! Z.Compare

  let normalise_input a b =
    let a = abs a in
    let b = abs b in
    if a > b then a, b else b, a
  ;;

  let rec extended_gcd a b =
    if b = zero
    then a, one, zero
    else (
      let d = a / b in
      let g, s, t = extended_gcd b (a mod b) in
      g, t, s - (d * t))
  ;;

  let extended_gcd_tco a b =
    let rec aux a b x1 y1 x2 y2 =
      if b = zero
      then a, x1, y1
      else (
        let quot = a / b in
        let rem = a mod b in
        aux b rem x2 y2 (x1 - (quot * x2)) (y1 - (quot * y2)))
    in
    aux a b one zero zero one
  ;;
end

module ExtendedEuclideanAlgorithmTests = struct
  module MakeTests (Gcd : sig
      val extended_gcd : Z.t -> Z.t -> Z.t * Z.t * Z.t
      val name : string
    end) =
  struct
    open Z
    open Z.Compare
    open Gcd

    let z = Z.of_int
    let pair a b = z a, z b

    (* Test basic GCD calculations *)
    let%test_unit "basic_gcd_test" =
      let test_cases =
        [ pair 35 15, z 5
        ; (* Common factors *)
          pair 45 10, z 5
        ; (* Different numbers, same GCD *)
          pair 101 13, z 1
        ; (* Coprime numbers *)
          pair 624 888, z 24
        ; (* Larger numbers *)
          pair 7 7, z 7 (* Equal numbers *)
        ]
      in
      List.iter test_cases ~f:(fun ((a, b), expected_gcd) ->
        let gcd, _, _ = extended_gcd a b in
        assert (gcd = expected_gcd))
    ;;

    (* Test Bézout's identity: ax + by = gcd *)
    let%test_unit "bezout_identity_test" =
      let test_cases = [ pair 35 15; pair 45 10; pair 101 13; pair 624 888; pair 7 7 ] in
      List.iter test_cases ~f:(fun (a, b) ->
        let gcd, x, y = extended_gcd a b in
        [%test_result: bool]
          ~expect:true
          ((a * x) + (b * y) = gcd)
          ~message:[%string "%{name} - Bezout Identity Test"])
    ;;

    (* Test specific known coefficients *)
    let%test_unit "known_coefficients_test" =
      let gcd, x, y = extended_gcd (z 35) (z 15) in
      assert (gcd = z 5);
      assert (x = z 1);
      assert (y = z (-2));
      let gcd, x, y = extended_gcd (z 101) (z 13) in
      assert (gcd = z 1);
      assert (x = z 4);
      assert (y = z (-31))
    ;;

    (* Test that GCD is always positive *)
    let%test_unit "positive_gcd_test" =
      let test_cases = [ pair 35 15; pair (-35) 15; pair 35 (-15); pair (-35) (-15) ] in
      List.iter test_cases ~f:(fun (a, b) ->
        let gcd, _, _ = extended_gcd (Z.abs a) (Z.abs b) in
        assert (gcd > zero))
    ;;

    (* Test with zero *)
    let%test_unit "zero_input_test" =
      let gcd, x, y = extended_gcd (z 10) Z.zero in
      assert (gcd = z 10);
      assert (x = one);
      assert (y = zero);
      let gcd, x, y = extended_gcd zero (z 10) in
      assert (gcd = z 10);
      assert (x = zero);
      assert (y = one)
    ;;

    (* Test large numbers *)
    let%test_unit "large_numbers_test" =
      let a = Z.of_string "987654321987654321987654321" in
      let b = Z.of_string "123456789123456789123456789" in
      let gcd, x, y = extended_gcd a b in
      assert (gcd > zero);
      assert ((a * x) + (b * y) = gcd)
    ;;

    (* Property-based tests *)
    let%test_unit "gcd_divides_both" =
      let test_cases = [ pair 35 15; pair 45 10; pair 101 13; pair 624 888 ] in
      List.iter test_cases ~f:(fun (a, b) ->
        let gcd, _, _ = extended_gcd a b in
        assert (Z.rem a gcd = zero);
        assert (Z.rem b gcd = zero))
    ;;

    (* Test GCD properties *)
    let%test_unit "gcd_properties" =
      let test_cases = [ pair 35 15; pair 45 10; pair 101 13 ] in
      List.iter test_cases ~f:(fun (a, b) ->
        let gcd1, _, _ = extended_gcd a b in
        let gcd2, _, _ = extended_gcd b a in
        (* GCD is commutative *)
        assert (gcd1 = gcd2);
        (* GCD with self is self *)
        let gcd_self, _, _ = extended_gcd a a in
        assert (gcd_self = Z.abs a))
    ;;

    (* Test really large numbers *)
    let%test_unit "very_large_numbers" =
      let a = Z.of_string "123456789123456789123456789123456789" in
      let b = Z.of_string "987654321987654321987654321987654321" in
      let gcd, x, y = extended_gcd a b in
      assert (gcd > zero);
      assert ((a * x) + (b * y) = gcd);
      (* Test that result divides both numbers *)
      assert (Z.rem a gcd = zero);
      assert (Z.rem b gcd = zero)
    ;;
  end

  module DefaultImplementationTest = MakeTests (struct
      let extended_gcd = ExtendedEuclideanAlgorithm.extended_gcd
      let name = "Non TCO Extended GCD"
    end)

  module TCOImplementationTest = MakeTests (struct
      let extended_gcd = ExtendedEuclideanAlgorithm.extended_gcd_tco
      let name = "TCO Extended GCD"
    end)
end

(* TODO: Implement benchmarks *)
