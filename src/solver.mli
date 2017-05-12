(* solver.mli -- find installable subsets of OPAM packages
   Copyright 2017 Inria
   author: Damien Doligez
*)

val solve :
  Vdd.u ->
  Package.t list ->
  ocaml:string ->
  pack:string ->
  vers:string ->
  (Vdd.t * string list)
(** [(v, l) = solve u packs ~ocaml ~pack ~vers]
    [v] is a VDD that represents the set of possible solutions for
    installing package [pack] at version [vers] in switch [ocaml].
    [l] is the list of all relevant packages for this install.
    The [u] and [packs] parameter should be the ones returned by
    {!Package.make}
*)
val schedule :
  Vdd.u ->
  Package.t list ->
  (string * string) list ->
  (string * string) list
(** [schedule u packs sol]
    Return the solution [sol], in an order that allows installing
    the packages one by one.
*)
