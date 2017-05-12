(* toposort.mli -- topological sorting with cycle-breaking
   Copyright 2017 Inria
   author: Damien Doligez
*)

val sort : ('a -> (string * string list)) -> 'a list -> 'a list
(** [sort f l]
    Sort [l] according to the relation given by [f]. Given an
    argument [x], [f] returns a pair [(name, deps)] where every
    pair [(name, dep)] is a vertex of the order ([name] >= [dep]).
    The elements of [l] are returned in some non-decreasing order.
    If there are cycles in the order, they are broken arbitrarily.
*)

