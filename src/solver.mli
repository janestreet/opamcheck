(* solver.mli -- find installable subsets of OPAM packages
   Copyright 2017 Inria
   author: Damien Doligez
*)

val solve :
  Package.u ->
  (string * string) list ->
  ocaml:string ->
  pack:string ->
  vers:string ->
  (string * string) list option
(** [solve u prev ~ocaml ~pack ~vers]
    Return a solution for installing [pack] at version [vers] that
    extends the solution [prev].
*)

exception Schedule_failure of (string * string) list * (string * string) list
(** Exception raised when [schedule] fails to find a suitable order.
    The arguments are: the schedule found so far and the set of remaining
    packages.
*)

val schedule :
  Package.u ->
  (string * string) list ->
  (string * string) list ->
  string * string ->
  (string * string) list
(** [schedule u prev sol target]
    Return the solution [sol], in an order that allows installing
    the packages one by one. [prev] must be a subset of [sol], and
    will be a prefix of the result. If [target] is reached before
    all packages are installed then the remaining packages are
    omitted from the solution.
*)
