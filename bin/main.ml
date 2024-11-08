open! Core
open Cow_of_ocaml [@@warning "-33"]
open Core_bench [@@warning "-33"]
open BinaryExponentiation

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"Non bin exp Pow" (fun () ->
           ignore (BinPow.non_bin_pow_rec_tco (Z.of_int 2) (Z.of_int 10000)))
       ; Bench.Test.create ~name:"Bin exp Pow" (fun () ->
           ignore (BinPow.bin_pow_rec (Z.of_int 2) (Z.of_int 10000)))
       ; Bench.Test.create ~name:"Non bin exp Pow TCO" (fun () ->
           ignore (BinPow.bin_pow_rec_tco (Z.of_int 2) (Z.of_int 10000)))
       ; Bench.Test.create ~name:"Bin exp loop Pow" (fun () ->
           ignore (BinPow.bin_pow_loop (Z.of_int 2) (Z.of_int 10000)))
       ])
;;
