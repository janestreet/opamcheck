(* package.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

open Util

type t = {
  name : string;
  version : string;
  lit : Minisat.Lit.t;
  dep_opt : string list;
  deps : Ast.package Ast.formula;
}

type u = {
  sat : Minisat.t;
  packs : t list;
  pack_map : t list Util.SM.t;
  lits : (string * Minisat.Lit.t) list Util.SM.t;
}

let find_lit u name vers = List.assoc vers (SM.find name u.lits)

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
         | None -> Log.warn "Warning in %s: version not found\n" name;
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

type context = {
  lits : (string * Minisat.Lit.t) list SM.t;
  cur_pack : string;
  cur_lit : Minisat.Lit.t;
  warn : bool;
}

let mk_or l1 l2 =
  let f acc cl = List.map ((@) cl) l2 @ acc in
  List.fold_left f [] l1

let mk_not l =
  let neg c = List.map (fun lit -> [Minisat.Lit.neg lit]) c in
  let f acc c = mk_or acc (neg c) in
  List.fold_left f [ [] ] l

let mk_impl_1 lit l =
  let nlit = Minisat.Lit.neg lit in
  List.map (fun c -> nlit :: c) l

let rec translate_form tratom c f =
  match f with
  | Ast.And (f1, f2) ->
     (translate_form tratom c f1) @ (translate_form tratom c f2)
  | Ast.List l ->
    List.flatten (List.map (translate_form tratom c) l)
  | Ast.Or (f1, f2) ->
     let l1 = translate_form tratom c f1 in
     let l2 = translate_form tratom c f2 in
     mk_or l1 l2
  | Ast.Not (f1) -> mk_not (translate_form tratom c f1)
  | Ast.Atom a -> tratom c a

let safe_atom c pack filter =
  try
    let vers = SM.find pack c.lits in
    let f acc (v, lit) = if filter v then lit :: acc else acc in
    [List.fold_left f [] vers]
  with Not_found ->
    if c.warn then Log.warn "Warning in %s: %s doesn't exist\n" c.cur_pack pack;
    [ [] ]

let translate_constraint pack c (comp, vers) =
  match comp with
  | Ast.Eq -> safe_atom c pack ((=) vers)
  | Ast.Ne -> safe_atom c pack ((<>) vers)
  | Ast.Lt -> safe_atom c pack (fun x -> Version.compare x vers < 0)
  | Ast.Le -> safe_atom c pack (fun x -> Version.compare x vers <= 0)
  | Ast.Gt -> safe_atom c pack (fun x -> Version.compare x vers > 0)
  | Ast.Ge -> safe_atom c pack (fun x -> Version.compare x vers >= 0)

let translate_package c p =
  let name = fst p in
  match snd p with
  | None -> safe_atom c name (fun x -> true)
  | Some f -> translate_form (translate_constraint name) c f

let translate_dep c d =
  mk_impl_1 c.cur_lit (translate_form translate_package c d)

let translate_conflict c pack =
  mk_impl_1 c.cur_lit (mk_not (translate_package c pack))

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
  mk_impl_1 c.cur_lit (ocv @ avail)

let rec union l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t -> if List.mem h l2 then union t l2 else union t (h :: l2)

let make ocaml_versions asts =
  let add_version vars (dir, ast) =
    try
      let n = get_name dir ast in
      let v = get_version n dir ast in
      let vv = try SM.find n vars with _ -> [] in
      SM.add n (v :: vv) vars
    with Not_found -> vars
  in
  let f accu (name, vers) = SM.add name vers accu in
  let vars = List.fold_left f SM.empty (Env.get ocaml_versions) in
  let vars = List.fold_left add_version vars asts in
  let genlit = ref 0 in
  let f v = (v, (incr genlit; Minisat.Lit.make !genlit)) in
  let cmp v1 v2 = Version.compare v2 v1 in
  let lits = SM.map (fun vs -> List.map f (List.sort cmp vs)) vars in
  let sat = Minisat.create () in
  let u = { sat; packs = []; pack_map = SM.empty; lits } in
  let conflict name v1 v2 =
    let l1 = find_lit u name v1 in
    let l2 = find_lit u name v2 in
    Minisat.add_clause_l sat [Minisat.Lit.neg l1; Minisat.Lit.neg l2]
  in
  let rec self_conflict name vers =
    match vers with
    | [] -> ()
    | h :: t -> List.iter (conflict name h) t; self_conflict name t
  in
  SM.iter self_conflict vars;
  let f (dir, ast) =
    let name = get_name dir ast in
    let version = get_version name dir ast in
    let cur_lit = find_lit u name version in
    let deps = get_depends ast in
    let opts = get_depopts ast in
    let conf = get_conflicts ast in
    let avail = get_available ast in
    let ocv = get_ocaml_version ast in
    let dep_opt = summarize_deps opts in
    let c = { lits; cur_pack = name ^ "." ^ version; cur_lit; warn = true } in
    let dep_constraint = translate_dep c deps in
    let conflicts = List.map (translate_conflict {c with warn = false}) conf in
    let available = translate_available c avail ocv in
    List.iter (Minisat.add_clause_l sat) dep_constraint;
    List.iter (Minisat.add_clause_l sat) available;
    List.iter (fun cnf -> List.iter (Minisat.add_clause_l sat) cnf) conflicts;
    { name; version; lit = cur_lit; dep_opt; deps }
  in
  let compiler_packs =
    let mk_comp version = {
      name = "compiler";
      version;
      lit = find_lit u "compiler" version;
      dep_opt = [];
      deps = Ast.List [];
    } in
    List.map mk_comp ocaml_versions
  in
  let packs = compiler_packs @ List.map f asts in
  let f map pack =
    let versions = try SM.find pack.name map with Not_found -> [] in
    SM.add pack.name (pack :: versions) map
  in
  let pack_map = List.fold_left f SM.empty packs in
  { sat; packs; pack_map; lits }

let show p =
  printf "pack = %s.%s\n" p.name p.version;
  printf "lit = %d\n" (Minisat.Lit.to_int p.lit);
  printf "dep_opt = [";
    List.iter (printf " %s") p.dep_opt;
  printf " ]\n";
  printf "-------------------------------------------------\n";
  ()
