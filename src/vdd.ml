(* vdd.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

type tree =
  | False
  | True
  | Node of {tag : int; var : int; sons : tree array}

type var = {
  name : string;
  id : int;
  vals : string array;
}

let get_tag = function
  | False -> 0
  | True -> 1
  | Node {tag; _} -> tag

let array_for_all2 f a1 a2 =
  if Array.length a1 <> Array.length a2 then
    raise (Invalid_argument "array_for_all2");
  let rec loop i =
    if i >= Array.length a1 then true
    else if f a1.(i) a2.(i) then loop (i+1)
    else false
  in
  loop 0

module WeakHashedtree =
struct
  type t = tree
  let equal x y =
    match x, y with
    | Node {var=varx; sons=sonsx}, Node {var=vary; sons=sonsy} ->
        varx = vary && array_for_all2 (==) sonsx sonsy
    | _ -> x == y
  let hash x =
    match x with
    | False -> 0
    | True -> 1
    | Node {var; sons} ->
       Array.fold_left (fun x y -> x + 93 * get_tag y) var sons
end

(* Weak hash table of all the nodes *)
module WT = Weak.Make (WeakHashedtree)

(* Because our VDDs are hash-consed we can use pointer equality to compare
   them. *)
let equal = (==)

(* All our functions will take a universe argument. The universe is the set of
   all variables, each with its set of possible values.
   Each VDD lives in a given universe, and VDDs from different universes
   must not be mixed. *)
type u = {
  var_names : (string, var) Hashtbl.t;
  var_nums : var array;
  cache : WT.t;
  mutable genstate : int;
}

(* A printing function for debugging *)
let node_to_string u n =
  match n with
  | False -> "0: False"
  | True -> "1: True"
  | Node {tag; var; sons} ->
     let b = Buffer.create 80 in
     Printf.bprintf b "%d: %s [" tag u.var_nums.(var).name;
     Array.iter2 (fun v s -> Printf.bprintf b " %s:%d" s (get_tag v)) sons
       u.var_nums.(var).vals;
     Printf.bprintf b " ]";
     Buffer.contents b

let mk_universe vars =
  let v1 = Array.of_list vars in
  let f id (name, vals) = {name; id; vals = Array.of_list vals} in
  let var_nums = Array.mapi f v1 in
  let var_names = (Hashtbl.create 997 : (string, var) Hashtbl.t) in
  let f v = Hashtbl.add var_names v.name v in
  Array.iter f var_nums;
  {var_names; var_nums; cache = WT.create 65537; genstate = 1}

let gen_tag u =
  u.genstate <- u.genstate + 1;
  if u.genstate = 0 then failwith "vdd.ml: tag numbers have wrapped around";
  u.genstate

let mk_node u var sons =
  assert (Array.length sons = Array.length u.var_nums.(var).vals);
  if Array.length sons = 0 then False
  else if Array.for_all ((==) sons.(0)) sons then sons.(0)
  else begin
    let arg = (Node {tag = gen_tag u; var; sons}) in
    let res = WT.merge u.cache arg in
    res
  end

let atom u var pred =
  let vv = Hashtbl.find u.var_names var in
  let f v = if pred v then True else False in
  let sons = Array.map f vv.vals in
  mk_node u vv.id sons

let mk_false u = False
let mk_true u = True

let is_false u v = v == False
let is_true u v = v == True

module Hashed_tree =
struct
  type t = tree
  let equal = (==)
  let hash = get_tag
end

module HT1 = Hashtbl.Make (Hashed_tree)

let memo1 f u x =
  let h = HT1.create 19 in
  let rec ff u x =
    try HT1.find h x
    with Not_found -> let v = f ff u x in HT1.add h x v; v
  in
  ff u x

let _not rec_not u x =
  match x with
  | False -> True
  | True -> False
  | Node {var; sons} -> mk_node u var (Array.map (rec_not u) sons)

let mk_not = memo1 _not

module Hashed_tree_pair =
struct
  type t = tree * tree
  let equal (x1, y1) (x2, y2)  = x1 == x2 && y1 == y2
  let hash (x, y) = 93 * get_tag x + get_tag y
end

module HT2 = Hashtbl.Make (Hashed_tree_pair)

let memo2 f u x y =
  let h = HT2.create 19 in
  let rec ff u x =
    try HT2.find h x
    with Not_found -> let v = f ff u x in HT2.add h x v; v
  in
  ff u (x, y)

let distribute f u p =
  match p with
  | (Node {var = varx; sons = sonsx} as x),
    (Node {var = vary; sons = sonsy} as y) ->
     if varx = vary then
       mk_node u varx (Array.map2 (fun x y -> f u (x, y)) sonsx sonsy)
     else if varx < vary then
       mk_node u varx (Array.map (fun xx -> f u (xx, y)) sonsx)
     else begin
       assert (varx > vary);
       mk_node u vary (Array.map (fun yy -> f u (x, yy)) sonsy)
     end
  | _ -> assert false

let _and rec_and u xy =
  match xy with
  | False, _ -> False
  | _, False -> False
  | True, y -> y
  | x, True -> x
  | x, y when x == y -> x
  | xy -> distribute rec_and u xy

let mk_and = memo2 _and

let _or rec_or u xy =
  match xy with
  | False, y -> y
  | x, False -> x
  | True, y -> True
  | x, True -> True
  | x, y when x == y -> x
  | xy -> distribute rec_or u xy

let mk_or = memo2 _or

let _impl rec_impl u xy =
  match xy with
  | False, y -> True
  | x, False -> mk_not u x
  | True, y -> y
  | x, True -> True
  | x, y when x == y -> True
  | xy -> distribute rec_impl u xy

let mk_impl = memo2 _impl

let _equiv rec_equiv u xy =
  match xy with
  | False, y -> mk_not u y
  | x, False -> mk_not u x
  | True, y -> y
  | x, True -> x
  | x, y when x == y -> True
  | xy -> distribute rec_equiv u xy

let mk_equiv = memo2 _equiv

let _nand rec_nand u xy =
  match xy with
  | False, y -> True
  | x, False -> True
  | True, y -> mk_not u y
  | x, True -> mk_not u x
  | x, y when x == y -> mk_not u x
  | xy -> distribute rec_nand u xy

let mk_nand = memo2 _nand

type counter = tree * int HT1.t

let add x y =
  assert (x >= 0 && y >= 0);
  if x + y < 0 then max_int else x + y

let count u x =
  let h = HT1.create 19 in
  let rec cnt x =
    try HT1.find h x
    with Not_found ->
      let res =
        match x with
        | False -> 0
        | True -> 1
        | Node {sons} -> Array.fold_left (fun acc n -> add acc (cnt n)) 0 sons
      in
      HT1.add h x res;
      res
  in
  ignore (cnt x);
  (x, h)

let get_count _u (x, h) =
  try HT1.find h x with Not_found -> assert false

let get_nth u (x, h) n =
  if n >= get_count u (x,h) then raise (Invalid_argument "Vdd.get_nth");
  let rec get x n accu =
    match x with
    | False -> assert false
    | True -> assert (n = 0); accu
    | Node {var; sons} ->
       let rec loop i n =
         let c = get_count u (sons.(i), h) in
         if n >= c then
           loop (i+1) (n - c)
         else begin
           let name = u.var_nums.(var).name in
           let v = u.var_nums.(var).vals.(i) in
           get sons.(i) n ((name, v) :: accu)
         end
       in
       loop 0 n
  in
  get x n []

let rec iter u f x acc =
  match x with
  | False -> ()
  | True -> f acc
  | Node {var; sons} ->
     let g i y =
       iter u f y ((u.var_nums.(var).name, u.var_nums.(var).vals.(i)) :: acc)
     in
     Array.iteri g sons

let iter u f x = iter u f x []

type t = tree

(************************************************)
(* For debugging *)

let _show rec_show u x =
  Printf.printf "%s\n" (node_to_string u x);
  match x with
  | False -> ()
  | True -> ()
  | Node {tag; var; sons} -> Array.iter (rec_show u) sons

let show = memo1 _show
