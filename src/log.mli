(* log.mli -- logging and warning
   Copyright 2017 Inria
   author: Damien Doligez
*)

val log_chan : Pervasives.out_channel
(** stdout *)

val log : ('a, unit, string, unit) format4 -> 'a
(** Write to stdout and flush. *)

val res_chan : Pervasives.out_channel
(** The results file *)

val res : ('a, unit, string, unit) format4 -> 'a
(** Write to the results file and flush. *)

val status_chan : Pervasives.out_channel
(** The status file *)

val status : ('a, unit, string, unit) format4 -> 'a
(** Write to the status file and flush. *)

val warn_chan : Pervasives.out_channel
(** The warnings file *)

val warn : ('a, unit, string, unit) format4 -> 'a
(** Write to the warnings file and flush. *)

val trace_chan : Pervasives.out_channel
(** The trace file *)

val trace : ('a, unit, string, unit) format4 -> 'a
(** Write to the trace file and flush. *)

val fatal : ('a, unit, string, 'b) format4 -> 'a
(** Write to stdout and exit. *)
