(* package.mli -- package data structure and functions
   Copyright 2017 Inria
   author: Damien Doligez
*)

type t = {
  name : string;
  version : string;
  dep_packs : string list;
  dep_opt : string list;
  dep_constraint : Vdd.t;
  conflicts : (string * Vdd.t) list;
  available : Vdd.t;
}

val make : string list -> (string * Ast.opam list) list -> (Vdd.u * t list)
(** [make ocaml_versions asts]
    Translate a list of ASTs into a list of package records.
*)

val show : Vdd.u -> t -> unit
(** Display the package's contents to stdout *)
