open! Core

module EuclideanGCD = struct
  open Z
  open Z.Compare

  let rec gcd a = function
    | b when b = Z.zero -> a
    | b -> gcd b (a mod b)
  ;;

  let lcm a b = a * b / gcd a b

  let binary_gcd a b =
    let rec aux a b =
      match b with
      | b when b = Z.zero -> a
      | _ ->
        let b = Z.shift_right b (Z.trailing_zeros b) in
        let a, b =
          match a, b with
          | a, b when a > b -> b, a
          | _ -> a, b
        in
        let b = b - a in
        aux a b
    in
    match a = Z.zero || b = Z.zero with
    | true -> logor a b
    | false ->
      let orig_a = a in
      let orig_b = b in
      let a = abs (shift_right a (trailing_zeros a)) in
      let b = abs b in
      let min_trailing_zeros =
        min (of_int (trailing_zeros orig_a)) (of_int (trailing_zeros orig_b))
      in
      shift_left (aux a b) (to_int min_trailing_zeros)
  ;;
end

module EuclideanGCDBench = struct
  open! Core
  open! Core_bench
  open Z
  open EuclideanGCD

  let generate_random_pair ~size =
    let a = Z.random_bits size in
    let b = Z.random_bits size in
    a, b
  ;;

  let generate_fibonacci_pair n =
    let rec aux a b count =
      if Int.(count = 0) then a, b else aux b Z.(a + b) Int.(count - 1)
    in
    aux Z.one Z.one n
  ;;

  let generate_power_of_two size = shift_left one size, shift_left one Int.(size / 2)

  (* Benchmark module *)
  let make_bench_test name pairs gcd_fn =
    Bench.Test.create ~name (fun () ->
      List.iter pairs ~f:(fun (a, b) -> ignore (gcd_fn a b)))
  ;;

  let random_numbers_benchmark ~size =
    let pairs = List.init 100 ~f:(fun _ -> generate_random_pair ~size) in
    [ make_bench_test (Printf.sprintf "Regular GCD (random %d bits)" size) pairs Z.gcd
    ; make_bench_test (Printf.sprintf "Binary GCD (random %d bits)" size) pairs binary_gcd
    ]
  ;;

  let fibonacci_benchmark ~size =
    let pairs = List.init 100 ~f:(fun i -> generate_fibonacci_pair Int.(size + i)) in
    [ make_bench_test (Printf.sprintf "Regular GCD (fibonacci n=%d)" size) pairs Z.gcd
    ; make_bench_test (Printf.sprintf "Binary GCD (fibonacci n=%d)" size) pairs binary_gcd
    ]
  ;;

  let power_of_two_benchmark ~size =
    let pairs = [ generate_power_of_two size ] in
    [ make_bench_test
        (Printf.sprintf "Regular GCD (power of two %d bits)" size)
        pairs
        Z.gcd
    ; make_bench_test
        (Printf.sprintf "Binary GCD (power of two %d bits)" size)
        pairs
        binary_gcd
    ]
  ;;

  let all_benchmarks =
    List.concat
      [ random_numbers_benchmark ~size:64 (* small numbers *)
      ; random_numbers_benchmark ~size:512 (* medium numbers *)
      ; random_numbers_benchmark ~size:1024 (* large numbers *)
      ; random_numbers_benchmark ~size:4096 (* very large numbers *)
      ; fibonacci_benchmark ~size:100 (* small fibonacci *)
      ; fibonacci_benchmark ~size:1000 (* large fibonacci *)
      ; power_of_two_benchmark ~size:64 (* small powers of 2 *)
      ; power_of_two_benchmark ~size:1024 (* large powers of 2 *)
      ]
  ;;

  let command =
    Command.basic
      ~summary:"Benchmark GCD implementations"
      Command.Let_syntax.(
        let%map_open () = return () in
        fun () -> ignore (Bench.bench all_benchmarks))
  ;;
end

module EuclideanGCDTests = struct
  open Z
  open Z.Compare

  let generate_coprime_pair ~size =
    let rec aux () =
      let a = Z.random_bits size in
      let b = Z.random_bits size in
      if Z.(gcd a b = one) then a, b else aux ()
    in
    aux ()
  ;;

  let generate_with_known_gcd ~size ~cur_gcd =
    let a, b = generate_coprime_pair ~size in
    Z.(a * cur_gcd, b * cur_gcd)
  ;;

  let generate_fibonacci_pair n =
    let rec aux a b count =
      if count = zero then a, b else aux b Z.(a + b) (count - one)
    in
    aux one one n
  ;;

  module MakeTests (Gcd : sig
      val gcd : Z.t -> Z.t -> Z.t
      val name : string
    end) =
  struct
    open Gcd

    let%test_unit "gcd_large_coprime_numbers" =
      for _ = 1 to 10 do
        let a, b = generate_coprime_pair ~size:1024 in
        [%test_result: bool]
          ~expect:true
          (gcd a b = Z.one)
          ~message:[%string "%{name} - GCD of coprime numbers should be 1"]
      done
    ;;

    let%test_unit "gcd_large_numbers_with_known_gcd" =
      for _ = 1 to 10 do
        let known_gcd = Z.random_bits 64 in
        let a, b = generate_with_known_gcd ~size:1024 ~cur_gcd:known_gcd in
        [%test_result: bool]
          ~expect:true
          (gcd a b = abs known_gcd)
          ~message:[%string "%{name} - GCD should match the known value"]
      done
    ;;

    let%test_unit "gcd_consecutive_fibonacci_numbers" =
      for i = 1 to 100 do
        let a, b = generate_fibonacci_pair (Z.of_int i) in
        [%test_result: bool]
          ~expect:true
          (gcd a b = one)
          ~message:[%string "%{name} - Consecutive Fibonacci numbers should be coprime"]
      done
    ;;

    let%test_unit "gcd_edge_cases" =
      let test_cases =
        [ (* Zero cases *)
          zero, zero, zero
        ; zero, one, one
        ; one, zero, one (* Power of 2 cases *)
        ; shift_left one 1024, shift_left one 512, shift_left one 512 (* Large powers *)
        ; pow (of_int 2) 1024, pow (of_int 2) 512, pow (of_int 2) 512
          (* Known large composites *)
        ; ( pow (of_int 2) 107 - one (* Mersenne prime M107 *)
          , pow (of_int 2) 127 - one (* Mersenne prime M127 *)
          , one )
          (* Perfect powers *)
        ; pow (of_int 12345) 67, pow (of_int 12345) 89, pow (of_int 12345) 67
        ]
      in
      List.iter test_cases ~f:(fun (a, b, expected) ->
        [%test_result: bool]
          ~expect:true
          (gcd a b = abs expected)
          ~message:[%string "%{name} - GCD test failed for inputs: a=%{a#Z} b=%{b#Z}"])
    ;;

    (* Test GCD properties *)
    let%test_unit "gcd_commutative_property" =
      for _ = 1 to 100 do
        let a = Z.random_bits 512 in
        let b = Z.random_bits 512 in
        [%test_result: bool]
          ~expect:true
          (gcd a b = gcd b a)
          ~message:[%string "%{name} - GCD should be commutative"]
      done
    ;;

    let%test_unit "gcd_associative_property" =
      for _ = 1 to 100 do
        let a = Z.random_bits 256 in
        let b = Z.random_bits 256 in
        let c = Z.random_bits 256 in
        [%test_result: bool]
          ~expect:true
          (gcd (gcd a b) c = gcd a (gcd b c))
          ~message:[%string "%{name} - GCD should be associative"]
      done
    ;;

    let%test_unit "gcd_bezout_identity" =
      for _ = 1 to 100 do
        let a = Z.random_bits 256 in
        let b = Z.random_bits 256 in
        let g = gcd a b in
        let _, x, y = Z.gcdext a b in
        [%test_result: bool]
          ~expect:true
          ((x * a) + (y * b) = g)
          ~message:[%string "%{name} - Bezout's identity should hold"]
      done
    ;;

    (* Performance stress test *)
    let%test_unit "gcd_stress_test_large_numbers" =
      let sizes = [ 1024; 2048; 4096; 8192 ] in
      List.iter sizes ~f:(fun size ->
        for _ = 1 to 5 do
          let a = Z.random_bits size in
          let b = Z.random_bits size in
          let _ = Z.gcd a b in
          ()
        done)
    ;;
  end

  module Regular_gcd_tests = MakeTests (struct
      let gcd = EuclideanGCD.gcd
      let name = "Regular GCD"
    end)

  (* Create your implementation modules like this: *)
  module Binary_gcd_tests = MakeTests (struct
      let gcd = EuclideanGCD.binary_gcd
      let name = "Binary GCD"
    end)
end
