(* lexer.mli -- lexical analyzer for OPAM's opam and url files
   Copyright 2017 Inria
   author: Damien Doligez
*)

val line : int ref
val token : Lexing.lexbuf -> Parser.token
