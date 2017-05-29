(* lexer.mli -- lexical analyzer for OPAM's opam and url files
   Copyright 2017 Inria
   author: Damien Doligez
*)

val token : Lexing.lexbuf -> Parser.token
