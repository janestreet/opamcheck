(*
  parsing_aux.ml
  Copyright 2017 Inria
  author: Damien Doligez
*)

let line = ref 1
let file = ref ""

let warn msg =
  Printf.eprintf "%s:%d Warning: %s\n" !file !line msg;
  flush stderr
