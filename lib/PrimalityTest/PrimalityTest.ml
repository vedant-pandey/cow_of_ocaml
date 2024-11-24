open! Core

(** Return true if number is prime and false otherwise *)
module PrimalityTests = struct
  (** Naive trial division *)
  let trial_division = function
    | x when x < 2 -> false
    | x ->
      Sequence.(
        range ~stop:`inclusive 2 Float.(to_int @@ sqrt @@ of_int @@ x)
        |> find_map ~f:(fun elem ->
          match x mod elem = 0 with
          | true -> Some false
          | false -> None))
      |> Option.value ~default:true
  ;;

  (* TODO: Implement Fermat's primality test *)
end

module PrimalityTests_UT = struct
  module MakePrimalityTest_UT (P : sig
      val is_prime : int -> bool
      val name : string
    end) =
  struct
    let%test_module "prime_tests" =
      (module struct
        print_endline [%string "BEGIN testing %{P.name}"]

        let%test "negative_numbers_are_not_prime" = not (P.is_prime (-5))
        let%test "zero_is_not_prime" = not (P.is_prime 0)
        let%test "one_is_not_prime" = not (P.is_prime 1)
        let%test "two_is_prime" = P.is_prime 2
        let%test "three_is_prime" = P.is_prime 3
        let%test "four_is_not_prime" = not (P.is_prime 4)

        let%test "composite_numbers_are_not_prime" =
          let composites = [ 4; 6; 8; 9; 10; 12; 14; 15; 16; 18; 20 ] in
          List.for_all composites ~f:(fun n -> not (P.is_prime n))
        ;;

        let%test "known_primes_are_identified" =
          let known_primes = [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31 ] in
          List.for_all known_primes ~f:P.is_prime
        ;;

        let%test "larger_primes_are_identified" =
          let larger_primes = [ 997; 1009; 1013; 1019 ] in
          List.for_all larger_primes ~f:P.is_prime
        ;;

        let%test "larger_composites_are_identified" =
          let larger_composites = [ 1000; 1002; 1004; 1006 ] in
          List.for_all larger_composites ~f:(fun n -> not (P.is_prime n))
        ;;

        let%test_unit "performance_test" =
          let start_time = Time_ns.now () in
          let limt = 1_000_000 in
          let test_numbers = List.range 1 limt in
          let _ = List.map test_numbers ~f:P.is_prime in
          let end_time = Time_ns.now () in
          let duration = Time_ns.diff end_time start_time in
          print_endline [%string "\nPerformance test for %{P.name}:\n"];
          print_endline
            [%string
              "Time to check first %{limt#Int} numbers: %{(Time_ns.Span.to_string \
               duration)}\n"];
          print_endline [%string "END testing %{P.name}"]
        ;;
      end)
    ;;
  end

  module TrialDivisionTest = MakePrimalityTest_UT (struct
      let is_prime = PrimalityTests.trial_division
      let name = "Trial division"
    end)
end
