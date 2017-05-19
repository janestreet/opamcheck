(* vdd.mli -- variadic decision diagrams
   Copyright 2017 Inria
   author: Damien Doligez
*)

type u (* type of universes *)
type t (* type of VDDs *)

exception Too_large
(** Raised when a VDD becomes too large to fit into memory. *)

val mk_universe : (string * string list) list -> u
(** Make a universe from a list of [(variable, values)] pairs. The
    variables are numbered in the order of the list.
*)

val atom : u -> string -> (string -> bool) -> t
(** [atom u var pred]
    Create a VDD that is a simple switch on variable [var]. The truth
    value of the VDD for each possible value of [var] is given by the
    [pred] function.
    @raise Not_found if [var] is not a variable of [u].
*)

val mk_false : u -> t
(** Return the VDD that is always false. *)
val mk_true : u -> t
(** Return the VDD that is always true. *)
val mk_not : u -> t -> t
(** [mk_not u v]
    Return the negation of [v]: a VDD that is true whenever [v] is false
    and false whenever [v] is true.
*)
val mk_and : u -> t -> t -> t
(** [mk_and u v1 v2]
    Return the conjunction of [v1] and [v2]: a VDD that is true when both
    [v1] and [v2] are true.
*)
val mk_or : u -> t -> t -> t
(** [mk_or u v1 v2]
    Return the disjunction of [v1] and [v2]: a VDD that is true when
    [v1] or [v2] is true.
*)
val mk_impl : u -> t -> t -> t
(** [mk_impl u v1 v2]
    Return the implication of [v1] and [v2]: a VDD that is true when
    [v1] is false or [v2] is true.
*)
val mk_equiv : u -> t -> t -> t
(** [mk_equiv u v1 v2]
    Return the equivalence of [v1] and [v2]: a VDD that is true when
    [v1] and [v2] are both true or both false.
*)

val mk_nand : u -> t -> t -> t
(** [mk_nand u v1 v2]
    Return the NAND (Sheffer stroke) of [v1] and [v2]: a VDD that is true
    when [v1] or [v2] is false.
*)

val is_false : u -> t -> bool
(** Return [true] iff the given VDD is always false. *)

val is_true : u -> t -> bool
(** Return [true] iff the given VDD is always true. *)

val iter : u -> ((string * string) list -> unit) -> t -> unit
(** [iter u f v]
    Call f on every solution of [v] in turn. A solution is a list of pairs
    (variable, value). The variables are in reverse order (compared to
    [mk_universe]).
*)

type counter
(** Type for the result of counting a VDD. *)

val count : u -> t -> counter
(** Count the VDD and return a [counter] for use with {!get_count}
    and {!get_nth}.
*)

val get_count : u -> counter -> int
(** Return the number of solutions of the VDD that was used to make
    the given [counter].
*)

val get_nth : u -> counter -> int -> (string * string) list
(** Return the [n]th solution of the VDD that was used to make the
    given [counter], counting from 0 in the same order as {!iter}.
    The variables are in reverse order (compared to [mk_universe]).
*)

val show : u -> t -> unit
(** Print the VDD to standard output. *)
