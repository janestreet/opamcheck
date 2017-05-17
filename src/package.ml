(* package.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

type t = {
  name : string;
  version : string;
  dep_packs : string list;
  dep_opt : string list;
  dep_constraint : Vdd.t;
  conflicts : (string * Vdd.t) list;
  available : Vdd.t;
}

module SM = Map.Make (String)

let rec get getter default l =
  match l with
  | [] -> default ()
  | field :: ll ->
     begin match getter field
     with
     | Some x -> x
     | None -> get getter default ll
     end

let get_name dir l =
  get (function Ast.Name n -> Some n | _ -> None)
      (fun () -> fst (Version.split_name_version dir))
      l

let get_version name dir l =
  get (function Ast.Version n -> Some n | _ -> None)
      (fun () ->
         match snd (Version.split_name_version dir) with
         | Some v -> v
         | None -> eprintf "Warning in %s: version not found\n" name;
                   raise Not_found
      )
      l

let get_depends l =
  get (function Ast.Depends form -> Some form | _ -> None)
      (fun () -> Ast.List [])
      l

let get_depopts l =
  get (function Ast.Depopts form -> Some form | _ -> None)
      (fun () -> Ast.List [])
      l

let get_conflicts l =
  get (function Ast.Conflicts list -> Some list | _ -> None)
      (fun () -> [])
      l

let get_available l =
  get (function Ast.Available form -> Some form | _ -> None)
      (fun () -> Ast.List [])
      l

let get_ocaml_version l =
  get (function Ast.Ocaml_version form -> Some form | _ -> None)
      (fun () -> Ast.List [])
      l

let rec summarize_deps deps =
  match deps with
  | Ast.And (d1, d2) -> summarize_deps d1 @ summarize_deps d2
  | Ast.List l -> List.fold_left (fun acc d -> summarize_deps d @ acc) [] l
  | Ast.Or (d1, d2) -> summarize_deps d1 @ summarize_deps d2
  | Ast.Not d -> summarize_deps d
  | Ast.Atom ((p, _)) -> [p]

type context = { u : Vdd.u; cur_pack : string; warn : bool; v : Vdd.t }

let rec translate_form tratom c f =
  match f with
  | Ast.And (f1, f2) ->
     Vdd.mk_and c.u (translate_form tratom c f1) (translate_form tratom c f2)
  | Ast.List l ->
      let f acc x = Vdd.mk_and c.u acc (translate_form tratom c x) in
      List.fold_left f (Vdd.mk_true c.u) l
  | Ast.Or (f1, f2) ->
     Vdd.mk_or c.u (translate_form tratom c f1) (translate_form tratom c f2)
  | Ast.Not (f1) -> Vdd.mk_not c.u (translate_form tratom c f1)
  | Ast.Atom a -> tratom c a

let safe_atom c pack filter =
  try
    let res = Vdd.atom c.u pack filter in
    res
  with Not_found ->
    if c.warn then begin
      eprintf "Warning in %s: %s doesn't exist\n" c.cur_pack pack;
      flush stderr;
    end;
    Vdd.mk_false c.u

let translate_constraint pack c (comp, vers) =
  match comp with
  | Ast.Eq ->
     safe_atom c pack (fun x -> x <> "." && x = vers)
  | Ast.Ne ->
     safe_atom c pack (fun x -> x <> "." && x <> vers)
  | Ast.Lt ->
     safe_atom c pack (fun x -> x <> "." && Version.compare x vers < 0)
  | Ast.Le ->
     safe_atom c pack (fun x -> x <> "." && Version.compare x vers <= 0)
  | Ast.Gt ->
     safe_atom c pack (fun x -> x <> "." && Version.compare x vers > 0)
  | Ast.Ge ->
     safe_atom c pack (fun x -> x <> "." && Version.compare x vers >= 0)

let translate_package c p =
  let name = fst p in
  match snd p with
  | None -> safe_atom c name ((<>) ".")
  | Some f -> translate_form (translate_constraint name) c f

let translate_dep c d =
  Vdd.mk_impl c.u c.v (translate_form translate_package c d)

let translate_conflict c ((name, constr) as pack) =
  (name, Vdd.mk_nand c.u c.v (translate_package c pack))

let comp_to_string = function
  | Ast.Eq -> "="
  | Ast.Lt -> "<"
  | Ast.Gt -> ">"
  | Ast.Le -> "<="
  | Ast.Ge -> ">="
  | Ast.Ne -> "!="

let translate_filter c filter =
  match filter with
  | var, Some constr -> translate_constraint var c constr
  | var, None -> translate_constraint var c (Ast.Eq, "true")

let translate_available c avail ocv =
  let ocv = translate_form (translate_constraint "ocaml-version") c ocv in
  let avail = translate_form translate_filter c avail in
  Vdd.mk_impl c.u c.v (Vdd.mk_and c.u ocv avail)

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t -> if List.mem h l2 then union t l2 else union t (h :: l2)

let sort_values l =
  let cmp v1 v2 =
    if v1 = v2 then 0
    else if v1 = "." then -1
    else if v2 = "." then 1
    else Version.compare v2 v1
  in
  List.sort cmp l

let make ocaml_versions asts =
  let add_version vars (dir, ast) =
    try
      let n = get_name dir ast in
      let v = get_version n dir ast in
      let d =
        summarize_deps (get_depends ast) @ summarize_deps (get_depopts ast)
      in
      let (vv, dd) = try SM.find n vars with _ -> (["."], []) in
      SM.add n (v :: vv, union d dd) vars
    with Not_found -> vars
  in
  let vars = List.fold_left add_version SM.empty asts in
  let incr m v =
    let n = try SM.find v m with Not_found -> 0 in
    SM.add v (n + 1) m
  in
  let incr_deps _ (_, deps) m = List.fold_left incr m deps in
  let weights = SM.fold incr_deps vars SM.empty in
  let cmp (v1, _) (v2, _) =
    let w1 = try SM.find v1 weights with Not_found -> 0 in
    let w2 = try SM.find v2 weights with Not_found -> 0 in
    Pervasives.compare w2 w1
  in
  let sorted =
    List.sort cmp (List.map (fun (v, (x, _)) -> (v, x)) (SM.bindings vars))
  in
  let sorted = Env.get ocaml_versions @ sorted in
  let sorted = List.map (fun (x, v) -> (x, sort_values v)) sorted in
  let u = Vdd.mk_universe sorted in
  let f u (dir, ast) =
    let name = get_name dir ast in
    let version = get_version name dir ast in
    let v = Vdd.atom u name ((=) version) in
    let c = { u; cur_pack = name ^ "." ^ version; v; warn=true } in
    let deps = get_depends ast in
    let opts = get_depopts ast in
    let conf = get_conflicts ast in
    let avail = get_available ast in
    let ocv = get_ocaml_version ast in
    let dep_packs = summarize_deps deps in
    let dep_opt = summarize_deps opts in
    let dep_constraint = translate_dep c deps in
    let conflicts = List.map (translate_conflict {c with warn = false}) conf in
    let available = translate_available c avail ocv in
    { name; version; dep_packs; dep_opt; dep_constraint; conflicts; available }
  in
  (u, List.map (f u) asts)
(*
  let packs = List.map (f u) asts in
  let cmp p1 p2 =
    if p1.name <> p2 .name then Pervasives.compare p1.name p2.name
    else Version.compare p1.version p2.version
  (u, List.sort cmp asts)
*)

let show u p =
  printf "pack = %s.%s\n" p.name p.version;
  printf "deps = [";
    List.iter (printf " %s") p.dep_packs;
  printf " ]\n";
  printf "dep_opt = [";
    List.iter (printf " %s") p.dep_opt;
  printf " ]\n";
  printf "constraint = "; Vdd.show u p.dep_constraint;
  printf "conflicts = [\n";
    List.iter (fun (n, v) -> printf "  %s: " n; Vdd.show u v) p.conflicts;
  printf "]\n";
  printf "available = "; Vdd.show u p.available;
  printf "-------------------------------------------------\n";
  ()
