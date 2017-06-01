(* env.mli -- handle OPAM metavariables (os, opam-version, etc)
   Copyright 2017 Inria
   author: Damien Doligez
*)

val compiler_to_ocaml_version : string -> string
(** Truncate the [compiler] variable to give the corresponding [ocaml-version]
*)

val get : string list -> (string * string list) list
(** [get ocaml]
    [ocaml] is the current OCaml version.
    Return the list of predefined variables with their values.
*)

val is_package : string * string -> bool
(** Return true iff the argument is not a predefined variable, or
    is "compiler".
*)
