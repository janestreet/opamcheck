(* version.mli -- manipulate and compare OPAM package version numbers
   Copyright 2017 Inria
   author: Damien Doligez
*)

val split_name_version : string -> string * string option
(** Take a package-with-version string and split it into package name
    and version. *)

val compare : string -> string -> int
(** [lt v1 v2]
    Return:
    * [-1] iff [v1] is less than [v2]
    * [0] iff [v1] is equal to [v2]
    * [1] iff [v1] is greater than [v2]
*)
