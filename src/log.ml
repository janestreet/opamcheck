(* log.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

let log_chan = Pervasives.stdout

let log fmt (* args *) =
  let f s = fprintf log_chan "%s" s; flush log_chan in
  kprintf f fmt (* args *)

let res_chan = open_out (Filename.concat Util.sandbox "results")

let res fmt (* args *) =
  let f s = fprintf res_chan "%s" s; flush res_chan in
  kprintf f fmt (* args *)

let status_chan = open_out (Filename.concat Util.sandbox "status")

let status fmt (* args *) =
  let f s = fprintf status_chan "%s" s; flush status_chan in
  kprintf f fmt (* args *)

let warn_chan = open_out (Filename.concat Util.sandbox "warnings")

let warn fmt (* args *) =
  let f s = fprintf warn_chan "%s" s; flush warn_chan in
  kprintf f fmt (* args *)

let trace_chan = open_out (Filename.concat Util.sandbox "trace")

let trace fmt (* args *) =
  let f s = fprintf trace_chan "%s" s; flush trace_chan in
  kprintf f fmt (* args *)

let fatal fmt (* args *) =
  let f s = fprintf log_chan "%s" s; exit 5 in
  kprintf f fmt (* args *)
