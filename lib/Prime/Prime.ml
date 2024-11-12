open! Core

module SieveAlgos = struct
  let sieve_of_eratosthenes limit =
    if limit <= 1
    then []
    else (
      let is_prime_arr = Array.create ~len:(limit + 1) true in
      is_prime_arr.(0) <- false;
      is_prime_arr.(1) <- false;
      List.(
        range ~stop:`inclusive 2 Float.(to_int (sqrt (of_int limit)))
        |> iter ~f:(fun i ->
          match is_prime_arr.(i) with
          | true when i * i <= limit ->
            range ~stop:`inclusive ~stride:i (i * i) limit
            |> iter ~f:(fun j -> is_prime_arr.(j) <- false)
          | _ -> ()));
      is_prime_arr
      |> Array.foldi ~init:[] ~f:(fun num acc ->
        function
        | true -> num :: acc
        | false -> acc)
      |> List.rev)
  ;;

  let sieve_of_eratosthenes_imperative limit =
    if limit <= 1
    then []
    else (
      let is_prime_arr = Array.create ~len:(limit + 1) true in
      is_prime_arr.(0) <- false;
      is_prime_arr.(1) <- false;
      let i = ref 2 in
      while !i <= Float.(to_int @@ sqrt @@ of_int limit) do
        if is_prime_arr.(!i)
        then (
          let j = ref (!i * !i) in
          while !j <= limit do
            is_prime_arr.(!j) <- false;
            j := !j + !i
          done);
        i := !i + 1
      done;
      is_prime_arr
      |> Array.foldi ~init:[] ~f:(fun num acc ->
        function
        | true -> num :: acc
        | false -> acc)
      |> List.rev)
  ;;
end

module SieveAlgosTest = struct
  module MakeTests (Sieve : sig
      val sieve_algo : int -> int list
      val name : string
    end) =
  struct
    let%test_unit "Correctness" =
      [ (* limit, expected_primes *)
        1, []
      ; 2, [ 2 ]
      ; 3, [ 2; 3 ]
      ; 4, [ 2; 3 ]
      ; 5, [ 2; 3; 5 ]
      ; 10, [ 2; 3; 5; 7 ]
      ; 20, [ 2; 3; 5; 7; 11; 13; 17; 19 ]
      ; 30, [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29 ]
      ; 50, [ 2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47 ]
      ; ( 100
        , [ 2
          ; 3
          ; 5
          ; 7
          ; 11
          ; 13
          ; 17
          ; 19
          ; 23
          ; 29
          ; 31
          ; 37
          ; 41
          ; 43
          ; 47
          ; 53
          ; 59
          ; 61
          ; 67
          ; 71
          ; 73
          ; 79
          ; 83
          ; 89
          ; 97
          ] )
      ; 0, []
      ; -1, []
      ]
      |> List.iter ~f:(fun (n, expect) ->
        [%test_result: int list]
          ~expect
          (Sieve.sieve_algo n)
          ~message:[%string "%{Sieve.name} - Get primes for limit=%{n#Int}"])
    ;;

    let%test_unit "memory efficiency" =
      let limit = 1_000_000 in
      let before_mem = Gc.stat () in
      let st = Core_unix.gettimeofday () in
      let _ = Sieve.sieve_algo limit in
      let en = Core_unix.gettimeofday () in
      let duration = Float.(to_int O.(1_000_000. * (en - st))) in
      print_endline [%string "%{Sieve.name} - Time - %{duration#Int} us"];
      let after_mem = Gc.stat () in
      let mem_words = after_mem.heap_words - before_mem.heap_words in
      (* Should use roughly (n/2) bits plus overhead *)
      [%test_result: bool]
        ~expect:true
        (mem_words <= 0)
        ~message:[%string "%{Sieve.name} - Memory test"]
    ;;
  end

  module EratosthenesTest = MakeTests (struct
      let sieve_algo = SieveAlgos.sieve_of_eratosthenes
      let name = "Eratosthenes"
    end)

  module EratosthenesImperativeTest = MakeTests (struct
      let sieve_algo = SieveAlgos.sieve_of_eratosthenes_imperative
      let name = "Imperative Eratosthenes"
    end)
end
