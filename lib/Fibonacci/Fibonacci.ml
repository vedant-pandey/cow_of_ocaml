open! Core

module Fibonacci = struct
  module Matrix = struct
    type t = (int * int) * (int * int)

    let fib_base_mat : t = (1, 1), (1, 0)

    let ( * ) (((a, b), (c, d)) : t) (((w, x), (y, z)) : t) : t =
      Int.(((a * w) + (b * y), (a * x) + (b * z)), ((c * w) + (d * y), (c * x) + (d * z)))
    ;;

    let pow b_mat exp =
      let rec aux ?(acc = fib_base_mat) b_mat pow =
        match pow with
        | 0 -> acc
        | exp ->
          (match exp land 1 with
           | 0 -> aux ~acc (b_mat * b_mat) (pow lsr 1)
           | _ -> aux ~acc:(acc * b_mat) (b_mat * b_mat) (pow lsr 1))
      in
      aux b_mat exp
    ;;
  end

  module Pair = struct
    type t =
      { first : int
      ; second : int
      }

    let ( * ) p1 p2 = (p1.first * p2.first) + (p1.second * p2.second)
  end

  let fibonacci_basic n =
    let rec aux ?(f = 0) ?(s = 1) = function
      | 0 -> f
      | 1 -> s
      | n -> aux ~f:s ~s:(f + s) (n - 1)
    in
    aux n
  ;;

  let fibonacci_matrix n =
    let (_, _), (_, f_n) = Matrix.(pow fib_base_mat n) in
    f_n
  ;;

  let rec fibonacci_fast_doubling_pair = function
    | 0 -> Pair.{ first = 0; second = 1 }
    | n ->
      let p = fibonacci_fast_doubling_pair (n lsr 1) in
      let c = p.first * ((2 * p.second) - p.first) in
      let d = Pair.(p * p) in
      (match n land 1 with
       | 0 -> { first = c; second = d }
       | _ -> { first = d; second = c + d })
  ;;

  let fibonacci_fast_doubling n =
    let Pair.{ first = f_n; _ } = fibonacci_fast_doubling_pair n in
    f_n
  ;;
end

module FibonacciEncoding = struct
  (* TODO: fix this implementation *)

  (*   let fib_numbers n = *)
  (*     let rec aux a b acc = if b > n then List.rev acc else aux b (a + b) (b :: acc) in *)
  (*     aux 1 2 [ 1 ] *)
  (*   ;; *)
  (*   let largest_fib_less_than n fibs = *)
  (*     List.fold fibs ~init:0 ~f:(fun acc x -> if x <= n then x else acc) *)
  (*   ;; *)
  (*   (* Convert list of bits to string *) *)
  (*   let bits_to_string bits = *)
  (*     List.rev bits |> List.map ~f:Int.to_string |> String.concat ~sep:"" *)
  (*   ;; *)
  (*   (* Main Fibonacci encoding function *) *)
  (*   let fibonacci_encode n = *)
  (*     if n < 1 *)
  (*     then raise (Invalid_argument "Input must be positive") *)
  (*     else if n = 1 *)
  (*     then "1" *)
  (*     else ( *)
  (*       let fibs = fib_numbers n in *)
  (*       let rec encode_helper remaining acc_bits = *)
  (*         if remaining = 0 *)
  (*         then acc_bits *)
  (*         else ( *)
  (*           let largest = largest_fib_less_than remaining fibs in *)
  (*           let smaller_fibs = List.filter fibs ~f:(fun x -> x < largest) in *)
  (*           let current_pos = List.length acc_bits in *)
  (*           let zeros_needed = max 0 (List.length smaller_fibs - current_pos) in *)
  (*           let padding = List.init zeros_needed ~f:(Fn.const 0) in *)
  (*           encode_helper (remaining - largest) ((1 :: padding) @ acc_bits)) *)
  (*       in *)
  (*       let result = encode_helper n [] in *)
  (*       bits_to_string (result @ [ 0 ])) *)
  (*   ;; *)
  (*   let%test_unit "Encode correctness" = *)
  (*     [ "1", fibonacci_encode 1, "Encoding 1" *)
  (*     ; "001", fibonacci_encode 2, "Encoding 2" *)
  (*     ; "0001", fibonacci_encode 3, "Encoding 3" *)
  (*     ; "00011", fibonacci_encode 4, "Encoding 4" *)
  (*     ; "00001", fibonacci_encode 5, "Encoding 5" *)
  (*     ; "00011", fibonacci_encode 6, "Encoding 6" *)
  (*     ; "00101", fibonacci_encode 7, "Encoding 7" *)
  (*     ; "000001", fibonacci_encode 8, "Encoding 8" *)
  (*     ; "000011", fibonacci_encode 9, "Encoding 9" *)
  (*     ; "000101", fibonacci_encode 10, "Encoding 10" *)
  (*     ; "0000001", fibonacci_encode 13, "Encoding 13" *)
  (*     ; "001011", fibonacci_encode 12, "Encoding 12 (5+7)" *)
  (*     ; "0001001", fibonacci_encode 16, "Encoding 16" *)
  (*     ; "0001011", fibonacci_encode 17, "Encoding 17 (13+4)" *)
  (*     ; "00000001", fibonacci_encode 21, "Encoding 21" *)
  (*     ; "0010101", fibonacci_encode 23, "Encoding 23 (13+8+2)" *)
  (*     ; "00100001", fibonacci_encode 29, "Encoding 29" *)
  (*     ] *)
  (*     |> List.iter ~f:(fun (expect, actual, name) -> *)
  (*       [%test_result: string] ~expect actual ~message:[%string "Test case for %{name}"]) *)
  (*   ;; *)
end

module FibonacciTests = struct
  open Fibonacci

  module MakeTest (Fib : sig
      val fib_func : int -> int
      val name : string
    end) =
  struct
    open Fib

    let rec gcd a b = if b = 0 then a else gcd b (a mod b)

    let%test_unit "Correctness" =
      [ 0, 0; 1, 1; 6, 8; 30, 832040 ]
      |> List.iter ~f:(fun (n, expect) ->
        [%test_result: int]
          ~expect
          (fib_func n)
          ~message:[%string "%{name} - Check fib for n=%{n#Int}"])
    ;;

    let%test_unit "Cassini identity" =
      [ 2, 1; 3, -1; 11, -1; 10, 1; 13, -1; 12, 1; 21, -1; 25, -1 ]
      |> List.iter ~f:(fun (n, expect) ->
        [%test_result: int]
          ~expect
          (let f_n = fib_func n in
           let f_n_minus_1 = fib_func (n - 1) in
           let f_n_plus_1 = fib_func (n + 1) in
           (f_n_plus_1 * f_n_minus_1) - (f_n * f_n))
          ~message:[%string "%{name} - Cassini identity at even n=%{n#Int}"])
    ;;

    let%test_unit "Addition rule" =
      [ 1, 2; 13, 34; 23, 53 ]
      |> List.iter ~f:(fun (n, k) ->
        [%test_result: bool]
          ~expect:true
          (let f_n_plus_k = fib_func (n + k) in
           let f_k = fib_func k in
           let f_n = fib_func n in
           let f_k_minus_1 = fib_func (k - 1) in
           let f_n_plus_1 = fib_func (n + 1) in
           f_n_plus_k = (f_n_plus_1 * f_k) + (f_k_minus_1 * f_n))
          ~message:[%string "%{name} - Adddition rule at n=%{n#Int} & k=%{k#Int}"])
    ;;

    let%test_unit "GCD identity" =
      [ 2, 3; 4, 10; 23, 34 ]
      |> List.iter ~f:(fun (m, n) ->
        [%test_result: bool]
          ~expect:true
          (let f_m = fib_func m in
           let f_n = fib_func n in
           gcd f_m f_n = fib_func @@ gcd m n)
          ~message:[%string "%{name} - GCD Identity at m=%{m#Int} n=%{n#Int}"])
    ;;
  end

  module BasicFibTest = MakeTest (struct
      let fib_func = fibonacci_basic
      let name = "Basic Fibonacci"
    end)

  module MatrixFibTest = MakeTest (struct
      let fib_func = fibonacci_matrix
      let name = "Matrix Fibonacci"
    end)

  module FastDoubleFibTest = MakeTest (struct
      let fib_func = fibonacci_fast_doubling
      let name = "Fast Double Fibonacci"
    end)
end

(* TODO: Add benchmarks *)
