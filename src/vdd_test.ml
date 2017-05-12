(*
  vdd_test.ml -- test some functions of vdd.ml
  Copyright 2017 Inria
  author: Damien Doligez
*)

let vars = [
  "x", ["1"; "2"; "3"; "4"];
  "y", ["0"; "1"];
  "z", ["a"; "b"; "c"];
  "t", ["A"; "B"];
]

let u = mk_universe vars

let a1 = atom u "x" (( > ) "3")
let a2 = atom u "y" (( = ) "1")
let a3 = atom u "z" (( = ) "c")

let f = ref (mk_not u (mk_and u a1 (mk_or u a2 a3)))

;; show u !f

let g = ref (mk_or u (mk_not u a1) (mk_and u (mk_not u a2) (mk_not u a3)))

;; show u !g

;; f := mk_false u
;; g := mk_false u
;; Gc.full_major ()

let h = ref (mk_not u (mk_and u a1 (mk_or u a2 a3)))
;; show u !h
;; count u !h

let print_sol l =
  List.iter (fun (var, v) -> Printf.printf " %s=%s" var v) l;
  Printf.printf "\n"

;; iter u print_sol !h
