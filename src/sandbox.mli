(* sandbox.mli -- call OPAM in a controlled environment
   Copyright 2017 Inria
   author: Damien Doligez
*)

type result =
  | OK
  | Failed of (string * string) list

val play_solution : (string * string) list -> result
