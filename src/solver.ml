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

let mk_indent i = String.make i '+'

let solve u l ~ocaml ~pack ~vers =
  let mp = make_pack_map l in
  let c = Vdd.atom u pack ((=) vers) in
  let ocaml_version = Env.compiler_to_ocaml_version ocaml in
  let compiler =
    Vdd.mk_and u (Vdd.atom u "compiler" ((=) ocaml))
      (Vdd.atom u "ocaml-version" ((=) ocaml_version))
  in
  let c = Vdd.mk_and u c compiler in
  let rec find_deps stk (c, visited, conflicts) pack =
    let stk = sprintf "%s/%s" stk pack in
eprintf "find: %s\n" stk; flush stderr;
    if SS.mem pack visited then
      (c, visited, conflicts)
    else begin
      let conf =
        match SM.find pack conflicts with
        | v -> v
        | exception Not_found -> Vdd.mk_true u
      in
      let find acc p = find_deps_vers acc pack p.version c visited in
      let (cc, deps, conflicts) =
        List.fold_left find (conf, [], conflicts) (SM.find pack mp)
      in
eprintf "find: %s (mk_and)\n" stk; flush stderr;
      let c = Vdd.mk_and u c cc in
eprintf "find: %s (update visited)\n" stk; flush stderr;
      let vis = SS.add pack visited in
eprintf "find: %s (recursive call)\n" stk; flush stderr;
      List.fold_left (find_deps stk) (c, vis, conflicts) deps
    end
  and find_deps_vers (cc, deps, conflicts) pack vers c visited =
eprintf "find: %s.%s\n" pack vers; flush stderr;
    let p = get_pack_vers mp pack vers in
    let (relevant, latent) =
      List.partition (fun (x, _) -> SS.mem x visited) p.conflicts
    in
eprintf "find: %s.%s (conf)\n" pack vers; flush stderr;
    let conf =
      List.fold_left (Vdd.mk_and u) (Vdd.mk_true u) (List.map snd relevant)
    in
eprintf "find: %s.%s (constr)\n" pack vers; flush stderr;
    let constr =
      Vdd.mk_and u
        (Vdd.mk_and u conf compiler)
        (Vdd.mk_and u p.dep_constraint p.available)
    in
eprintf "find: %s.%s (check)\n" pack vers; flush stderr;
    let check = Vdd.mk_and u constr (Vdd.atom u pack ((=) vers)) in
eprintf "find: %s.%s (test check)\n" pack vers; flush stderr;
    if Vdd.is_false u check || Vdd.is_false u (Vdd.mk_and u c check) then begin
      (Vdd.mk_and u cc (Vdd.atom u pack ((<>) vers)), deps, conflicts)
    end else begin
      let add conflicts (n, c) =
        match SM.find n conflicts with
        | v -> SM.add n (Vdd.mk_and u v c) conflicts
        | exception Not_found -> SM.add n c conflicts
      in
eprintf "find: %s.%s (add latent conflicts)\n" pack vers; flush stderr;
      let conf = List.fold_left add conflicts latent in
eprintf "find: %s.%s (final mk_and)\n" pack vers; flush stderr;
      (Vdd.mk_and u cc constr, p.dep_packs @ deps, conf)
    end
  in
  let (cc, deps, conflicts) =
    find_deps_vers (Vdd.mk_true u, [], SM.empty) pack vers c SS.empty
  in
  let c = Vdd.mk_and u c cc in
  let vis = SS.singleton pack in
  let (c, visited, _) =
    List.fold_left (find_deps pack) (c, vis, conflicts) deps
  in
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
