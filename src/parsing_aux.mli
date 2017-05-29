(*
  parsing_aux.mli -- auxiliary functions for lexing and parsing
  Copyright 2017 Inria
  author: Damien Doligez
*)

val line : int ref
val file : string ref

val warn : string -> unit
