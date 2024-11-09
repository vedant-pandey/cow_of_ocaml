open! Core

let command =
  Command.group
    ~summary:"Benchmark runner"
    [ "bin-exp", BinaryExponentiation.BinPowBench.command
    ; "euc-gcd", EuclideanGCD.EuclideanGCDBench.command
    ]
;;

let () = Command_unix.run command
