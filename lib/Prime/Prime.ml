open! Core

module SieveAlgos = struct
  let sieve_of_eratosthenes limit =
    if limit <= 1
    then []
    else (
      let is_prime_arr = Array.create ~len:(limit + 1) true in
      is_prime_arr.(0) <- false;
      is_prime_arr.(1) <- false;
      Sequence.(
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

  let sieve_of_eratosthenes_list limit =
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

  let segmented_sieve_of_eratosthenes ?(s = 10000) limit =
    if limit <= 1
    then []
    else (
      let lim_sqrt = Float.(to_int @@ sqrt @@ of_int limit) in
      let is_prime_arr = Array.create ~len:(lim_sqrt + 2) 'Y' in
      let prime_list = ref [] in
      let i = ref 2 in
      while !i <= lim_sqrt do
        if phys_equal is_prime_arr.(!i) 'Y'
        then (
          prime_list := !i :: !prime_list;
          let j = ref (!i * !i) in
          while !j <= lim_sqrt do
            is_prime_arr.(!j) <- 'N';
            j := !j + !i
          done;
          i := !i + 1)
      done;
      let k = ref 0 in
      let block = Array.create ~len:s 'Y' in
      while !k * s <= limit do
        Array.fill ~pos:0 ~len:s block 'Y';
        let st = !k * s in
        List.iter !prime_list ~f:(fun p ->
          let st_idx = (st + p - 1) / p in
          let j = ref ((Int.max st_idx p * p) - st) in
          while !j < s do
            block.(!j) <- 'N';
            j := !j + p
          done);
        if !k = 0
        then (
          block.(0) <- 'N';
          block.(1) <- 'N');
        k := !k + 1
      done;
      [])
  ;;
end

module SieveAlgosTest = struct
  module MakeSieveTests (Sieve : sig
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
      let limit = 100_000 in
      let before_mem = Gc.stat () in
      let st = Core_unix.gettimeofday () in
      let _ = Sieve.sieve_algo limit in
      let en = Core_unix.gettimeofday () in
      let duration = Float.(to_int O.(1_000_000. * (en - st))) in
      print_endline [%string "%{Sieve.name} - Time - %{duration#Int} us"];
      let after_mem = Gc.stat () in
      let mem_words = after_mem.heap_words - before_mem.heap_words in
      [%test_result: bool]
        ~expect:true
        (mem_words <= 0)
        ~message:[%string "%{Sieve.name} - Memory test"]
    ;;
  end

  module EratosthenesTest = MakeSieveTests (struct
      let sieve_algo = SieveAlgos.sieve_of_eratosthenes
      let name = "Eratosthenes"
    end)

  module EratosthenesListTest = MakeSieveTests (struct
      let sieve_algo = SieveAlgos.sieve_of_eratosthenes_list
      let name = "Eratosthenes(using list)"
    end)

  module EratosthenesImperativeTest = MakeSieveTests (struct
      let sieve_algo = SieveAlgos.sieve_of_eratosthenes_imperative
      let name = "Imperative Eratosthenes"
    end)
  (* module SegmentedEratosthenesTest_100000 = MakeSieveTests (struct *)
  (*     let sieve_algo = SieveAlgos.segmented_sieve_of_eratosthenes ~s:1000 *)
  (*     let name = "Segmented Sieve - 100000" *)
  (*   end) *)
  (* module SegmentedEratosthenesTest_10000 = MakeSieveTests (struct *)
  (*     let sieve_algo = SieveAlgos.segmented_sieve_of_eratosthenes ~s:100 *)
  (*     let name = "Segmented Sieve - 10000" *)
  (*   end) *)
end

(* TODO: Implement benchmarks *)
