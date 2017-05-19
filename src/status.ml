(* status.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

type step =
  | Solve of { max : int; cur_pack : string }
  | Install of { total : int; cur : int; cur_pack : string }

type t = {
  mutable ocaml : string;
  mutable pack_total : int;
  mutable pack_done : int;
  mutable pack_cur : string;
  mutable step : step;
}

let cur = {
  ocaml = "";
  pack_total = 0;
  pack_done = 0;
  pack_cur = "";
  step = Solve { max = 0; cur_pack = "" };
}

let sandbox = Sys.getenv "OPCSANDBOX"
let stchan = open_out (Filename.concat sandbox "status")

let show () =
  if Sys.file_exists (Filename.concat sandbox "stop") then begin
    fprintf stchan "\nSTOPPED BY USER";
    Pervasives.exit 10;
  end;
  fprintf stchan "\r%s %d/%d %s / "
    cur.ocaml cur.pack_done cur.pack_total cur.pack_cur;
  begin match cur.step with
  | Solve { max; cur_pack } ->
     fprintf stchan "Solve ";
     if max = max_int then
       fprintf stchan "*"
     else
       fprintf stchan "%d" max
     ;
     fprintf stchan " %s" cur_pack;
  | Install { cur; total; cur_pack } ->
     fprintf stchan "Install %d/%d %s" cur total cur_pack
  end;
  fprintf stchan "   ####";
  flush stchan
