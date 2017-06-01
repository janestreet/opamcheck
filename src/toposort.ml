(* toposort.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Util

let find_minimal rel inv =
  let best = ref "" in
  let best_len = ref max_int in
  let best_fanout = ref (-1) in
  let f name deps =
    let len = List.length deps in
    let fanout = List.length (SM.find name rel) in
    if len < !best_len || len = !best_len && fanout > !best_fanout then begin
      best := name;
      best_len := len;
      best_fanout := fanout;
    end
  in
  try SM.iter f inv; !best with Exit -> !best

let remove_dep name (inv, accu) dep =
  match List.filter ((<>) name) (SM.find dep inv) with
  | [] -> (SM.remove dep inv, dep :: accu)
  | l -> (SM.add dep l inv, accu)
  | exception Not_found -> (inv, accu)
    (* This happens when a loop was broken *)

let sort extract l =
  let add_presence name m = if SM.mem name m then m else SM.add name [] m in
  let add_inv name inv dep =
    let x = try SM.find dep inv with Not_found -> [] in
    SM.add dep (name :: x) inv
  in
  let add (index, rel, inv) x =
    let (name, deps) = extract x in
    let index = SM.add name x index in
    let rel = SM.add name deps rel in
    let inv = List.fold_left (add_inv name) (add_presence name inv) deps in
    (index, rel, inv)
  in
  let (index, rel, inv) = List.fold_left add (SM.empty, SM.empty, SM.empty) l in
  let p name l =
    l = []
  in
  let (leaves, inv) = SM.partition p inv in
  let leaves = List.map fst (SM.bindings leaves) in
  let rec loop leaves inv accu =
    match leaves with
    | [] ->
       if SM.is_empty inv then accu else begin
         let break = find_minimal rel inv in
         loop [break] (SM.remove break inv) accu
       end
    | h :: t ->
       begin match SM.find h index with
       | x ->
          let (inv, l) =
            List.fold_left (remove_dep h) (inv, t) (SM.find h rel)
          in
          loop l inv (x :: accu)
       | exception Not_found -> loop t inv accu
         (* This happens when some package depends on a missing package. *)
       end
  in
  loop leaves inv []
