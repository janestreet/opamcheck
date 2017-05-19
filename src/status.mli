(* status.mli -- display current status
   Copyright 2017 Inria
   author: Damien Doligez
*)

type step =
  | Solve of { max : int; cur_pack : string }
  | Install of { total : int; cur : int; cur_pack : string }

type t = {
  mutable ocaml : string;
  mutable pack_total : int;
  mutable pack_done : int;
  mutable pack_cur : string;
  mutable step : step;
}

val cur : t

val show : unit -> unit
