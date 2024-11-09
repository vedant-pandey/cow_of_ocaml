open! Core
open Cow_of_ocaml [@@warning "-33"]
open Core_bench [@@warning "-33"]

let () =
  (* TODO: use flags to run specific benchmarks *)

  (* NOTE: SECTION: Binary exponentiation *)
  let open BinaryExponentiation in
  let bin_pow_test_suite =
    [ "Bin exp pow", BinPow.bin_pow_rec
    ; "Non bin exp pow", BinPow.non_bin_pow_rec_tco
    ; "Bin pow rec TCO", BinPow.bin_pow_rec_tco
    ; "Bin pow imperative", BinPow.bin_pow_loop
    ]
  in
  let iters = [ 10; 100; 1000; 10000; 100000 ] in
  Command_unix.run
    (Bench.make_command
       (iters
        |> List.map ~f:(fun iter ->
          bin_pow_test_suite
          |> List.map ~f:(fun (name, func) ->
            Bench.Test.create ~name:[%string "%{name}-%{iter#Int}"] (fun () ->
              ignore (func (Z.of_int 2) (Z.of_int iter)))))
        |> List.fold ~init:[] ~f:(fun acc elem -> List.append elem acc)))
;;
