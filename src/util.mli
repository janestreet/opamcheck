(* util.mli -- utilities
   Copyright 2017 Inria
   author: Damien Doligez
*)

val sandbox : string

module SM : Map.S with type key = string
module SPM : Map.S with type key = string * string

module SS : Set.S with type elt = string
module SPS : Set.S with type elt = string * string
module SPLS : Set.S with type elt = (string * string) list
