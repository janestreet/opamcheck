(* solver.ml -- find installable subsets of OPAM packages
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

open Package

module SM = Map.Make (String)
module SS = Set.Make (String)

let make_pack_map l =
  let f map pack =
    let versions = try SM.find pack.name map with Not_found -> [] in
    SM.add pack.name (pack :: versions) map
  in
  List.fold_left f SM.empty l

let get_pack_vers mp pack vers =
  List.find (fun x -> x.version = vers) (SM.find pack mp)

let solve u l ~ocaml ~pack ~vers =
  let mp = make_pack_map l in
  let c = Vdd.atom u pack ((=) vers) in
  let ocaml_version = Env.compiler_to_ocaml_version ocaml in
  let compiler =
    Vdd.mk_and u (Vdd.atom u "compiler" ((=) ocaml))
      (Vdd.atom u "ocaml-version" ((=) ocaml_version)) in
  let c = Vdd.mk_and u c compiler in
  let rec find_deps_vers (c, visited) pack vers =
printf "solve: find_deps_vers: %s.%s\n" pack vers; flush stdout;
    let p = get_pack_vers mp pack vers in
    let cc =
      Vdd.mk_and u compiler (Vdd.mk_and u p.dep_constraint p.available)
    in
    let check = Vdd.mk_and u cc (Vdd.atom u pack ((=) vers)) in
    if Vdd.is_false u check || Vdd.is_false u (Vdd.mk_and u c check) then begin
printf "unavailable\n"; flush stdout;
      (Vdd.mk_and u c (Vdd.atom u pack ((<>) vers)), visited)
    end else begin
printf "available... visiting deps\n"; flush stdout;
      let c = Vdd.mk_and u c cc in
      List.fold_left find_deps_name (c, visited) p.dep_packs
    end
  and find_deps_name (c, visited) pack =
    if SS.mem pack visited then
      (c, visited)
    else begin
printf "solve: pack: %s\n" pack; flush stdout;
      let find acc p = find_deps_vers acc pack p.version in
      List.fold_left find (c, SS.add pack visited) (SM.find pack mp)
    end
  in
printf "solve: finding deps for %s.%s\n" pack vers; flush stdout;
  let (c, visited) = find_deps_vers (c, SS.singleton pack) pack vers in
  let add_conflicts pack c =
    let add c p =
      let relevant = List.filter (fun (x, _) -> SS.mem x visited) p.conflicts in
      List.fold_left (Vdd.mk_and u) c (List.map snd relevant)
    in
    List.fold_left add c (SM.find pack mp)
  in
printf "solve: adding conflicts for %s.%s\n" pack vers; flush stdout;
  let c = SS.fold add_conflicts visited c in
printf "solve: done\n"; flush stdout;
  (c, SS.elements visited)

exception Schedule_failure of (string * string) list

let schedule u packs sol =
  let present name vers = Vdd.atom u name ((=) vers) in
  let add_sol s (name, vers) = if vers = "." then s else SM.add name vers s in
  let sols = List.fold_left add_sol SM.empty sol in
  let add_pack m p =
    match SM.find p.name sols with
    | vers when vers = p.version -> SM.add p.name p m
    | _ -> m
    | exception Not_found -> m
  in
  let index = List.fold_left add_pack SM.empty packs in
  let is_compat (name, vers) pres remain1 remain2 =
    let pack = SM.find name index in
    let inst = Vdd.mk_and u pres (present name vers) in
    Vdd.is_true u (Vdd.mk_impl u inst pack.dep_constraint)
    && List.for_all (fun (x, _) -> not (List.mem x pack.dep_opt)) remain1
    && List.for_all (fun (x, _) -> not (List.mem x pack.dep_opt)) remain2
  in
  let rec find_compat pres l acc =
    match l with
    | [] -> raise (Schedule_failure acc)
    | h :: t ->
       if is_compat h pres t acc then
         (h, List.rev_append acc t)
       else
         find_compat pres t (h :: acc)
  in
  let rec loop inst vinst remain =
    if remain = [] then List.rev inst else begin
      let ((name, vers) as p, l) = find_compat vinst remain [] in
      loop (p :: inst) (Vdd.mk_and u vinst (present name vers)) l
    end
  in
  let useful (name, vers) =
    name <> "ocaml-version" && name <> "compiler" && vers <> "."
  in
  loop [] (Vdd.mk_true u) (List.rev (List.filter useful sol))