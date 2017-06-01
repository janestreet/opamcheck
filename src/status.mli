(* status.mli -- display current status
   Copyright 2017 Inria
   author: Damien Doligez
*)

type step =
  | Read of string
  | Cache
  | Solve of int
  | Install of { stored : bool; total : int; cur : int; cur_pack : string }

type t = {
  mutable ocaml : string;
  mutable pack_ok : int;
  mutable pack_done : int;
  mutable pack_total : int;
  mutable pack_cur : string;
  mutable step : step;
}

val cur : t

val show : unit -> unit
val show_result : char -> unit

val printf : ('a, out_channel, unit) format -> 'a

val flush : unit -> unit
