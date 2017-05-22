(* main.ml -- main program for opamcheck
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

let parse_opam file lb =
  Lexer.line := 1;
  try Parser.opam Lexer.token lb
  with
  | Parser.Error ->
     eprintf "\"%s\":%d -- syntax error\n" file !Lexer.line; exit 2
  | Failure msg ->
     eprintf "\"%s\":%d -- lexer error: %s\n" file !Lexer.line msg;
     exit 2

let parse_file dir file =
  Status.(cur.step <- Read file; show ());
  let ic = open_in file in
  let lb = Lexing.from_channel ic in
  let res = try parse_opam file lb with _ -> [] in
  close_in ic;
  (dir, res)

let fold_opam_files f accu dir =
  let rec dig dir accu name =
    let fullname = Filename.concat dir name in
    if name = "opam" then begin
      f accu (Filename.basename dir) fullname
    end else if Sys.is_directory fullname then
      Array.fold_left (dig fullname) accu (Sys.readdir fullname)
    else
      accu
  in
  dig dir accu "."

let sandbox = Sys.getenv "OPCSANDBOX"

let repo = Filename.concat sandbox "opam-repository"

type status = {
  mutable finished : bool;
  mutable ok : int;
  mutable failed : int;
  mutable forbid : Vdd.t;
}

module SS = Set.Make (String)

let read_lines file =
  if Sys.file_exists file then begin
    let ic = open_in file in
    let rec loop set =
      match input_line ic with
      | s -> loop (SS.add s set)
      | exception End_of_file -> set
    in
    let result = loop SS.empty in
    close_in ic;
    result
  end else
    SS.empty

module SM = Map.Make (String)

module StringPair = struct
  type t = string * string
  let compare = Pervasives.compare
end

module SPM = Map.Make (StringPair)

module StringTriple = struct
  type t = string * string * string
  let compare = Pervasives.compare
end

module STM = Map.Make (StringTriple)

module StringPairList = struct
  type t = (string * string) list
  let compare l1 l2 =
    let len1 = List.length l1 in
    let len2 = List.length l2 in
    if len1 > len2 then -1
    else if len1 < len2 then 1
    else Pervasives.compare l1 l2
end

module SPLS = Set.Make (StringPairList)

let cache = ref SPLS.empty

type progress = {
  mutable statuses : status STM.t;
  mutable num_done : int;
  mutable num_ok : int;
}

let make_failure u l =
  let f v (name, vers) = Vdd.mk_or u v (Vdd.atom u name ((<>) vers)) in
  List.fold_left f (Vdd.mk_false u) l

let get_status u p comp name vers =
  match STM.find (comp, name, vers) p.statuses with
  | r -> r
  | exception Not_found ->
     let r = { finished = false; ok = 0; failed = 0; forbid = Vdd.mk_true u } in
     p.statuses <- STM.add (comp, name, vers) r p.statuses;
     r

let record_finished u p comp name vers =
  let r = get_status u p comp name vers in
  if not r.finished then p.num_done <- p.num_done + 1;
  r.finished <- true

let record_ok u p comp l =
  let add_ok (name, vers) =
    let r = get_status u p comp name vers in
    if not r.finished then begin
      p.num_done <- p.num_done + 1;
      p.num_ok <- p.num_ok + 1;
    end;
    r.finished <- true;
    r.ok <- r.ok + 1;
  in
  let rec loop l =
    cache := SPLS.add l !cache;
    match l with
    | [] -> ()
    | h :: t -> add_ok h; loop t
  in
  loop l

let record_failed u p comp l =
  match l with
  | [] -> assert false
  | (name, vers) :: t ->
     let r = get_status u p comp name vers in
     r.failed <- r.failed + 1;
     r.forbid <- Vdd.mk_and u r.forbid (make_failure u l);
     record_ok u p comp t

type result =
  | Fail            (* failed *)
  | Running         (* not finished yet or inconclusive results *)
  | Unavailable     (* not installable *)
  | OK              (* ran OK *)
  | Empty           (* does not exist *)

let result_to_string r =
  match r with
  | Fail -> "FAIL"
  | Running -> " .. "
  | Unavailable -> "UNAV"
  | OK -> " OK "
  | Empty -> " -- "

let output_results p =
  let accum (comp, name, vers) r m =
    let st =
      if r.ok > 0 then OK
      else if r.finished && r.failed = 0 then Unavailable
      else if r.finished then Fail
      else Running
    in
    let prev = try SPM.find (name, vers) m with Not_found -> [] in
    SPM.add (name, vers) ((comp, st) :: prev) m
  in
  let statuses = STM.fold accum p.statuses SPM.empty in
  let summarize l =
    let l = List.sort (fun (v1, _) (v2, _) -> Version.compare v2 v1) l in
    let latest =
      match l with
      | [] -> assert false
      | (_, st) :: _ -> st
    in
    if List.exists (fun (_, st) -> st = OK) l then latest else Running
  in
  let statuses = SPM.map summarize statuses in
  let l = SPM.bindings statuses in
  let cmp (name1, st1) (name2, st2) =
    match Pervasives.compare st2 st1 with
    | 0 -> Pervasives.compare name1 name2
    | n -> n
  in
  let l = List.sort cmp l in
  let oc = open_out (Filename.concat (Sys.getenv "OPCSANDBOX") "results") in
  let print ((name, vers), st) =
    if st <> Running then begin
      fprintf oc "%s %s.%s\n" (result_to_string st) name vers
    end
  in
  match List.iter print l with
  | _ -> close_out oc
  | exception e -> close_out oc

let find_cached_sol u packs comp name vers =
  Status.(cur.step <- Cache; show ());
  let check p = p.Package.name = name && p.Package.version = vers in
  let p = List.find check packs in
  let result = ref None in
  let check sol =
    if List.for_all (fun (n, _) -> n <> name) sol then begin
      let sol = (name, vers) :: sol in
      let env =
        ("ocaml-version", Env.compiler_to_ocaml_version comp)
        :: sol
      in
      let eval_var v = try List.assoc v env with Not_found -> "." in
      if Vdd.eval u p.Package.dep_constraint eval_var
         && Vdd.eval u p.Package.available eval_var
         && List.for_all (fun (_, v) -> Vdd.eval u v eval_var)
              p.Package.conflicts
      then begin
        result := Some sol;
        raise Exit;
      end
    end
  in
  (try SPLS.iter check !cache with Exit -> ());
  !result

let test_comp_pack first u excludes packs progress comp pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  if SS.mem name excludes then
    (get_status u progress comp name vers).finished <- true;
  if not (get_status u progress comp name vers).finished then begin
    Status.(
      cur.ocaml <- comp;
      cur.pack_cur <- sprintf "%s.%s" name vers;
      cur.pack_ok <- progress.num_ok;
      cur.pack_done <- progress.num_done;
    );
    begin match find_cached_sol u packs comp name vers with
    | None ->
       let (sols, l) = Solver.solve u packs ~ocaml:comp ~pack:name ~vers in
       let r = get_status u progress comp name vers in
       let sols = Vdd.mk_and u sols r.forbid in
       if Vdd.is_false u sols then begin
         record_finished u progress comp name vers
       end else begin
         let ct = Vdd.count u sols in
         let n = Vdd.get_count u ct in
         assert (n > 0);
         let r = if first then 0 else Random.int (min 0x3FFFFFFF n) in
         let sol = Vdd.get_nth u ct r in
         match Solver.schedule u packs sol with
         | sched ->
            let sched = List.rev sched in
            begin match Sandbox.play_solution sched with
            | Sandbox.OK -> record_ok u progress comp sched
            | Sandbox.Failed l -> record_failed u progress comp l
            end
         | exception Solver.Schedule_failure _ ->
            eprintf "Schedule failed !\n"
       end
    | Some sol ->
       begin match Sandbox.play_solution sol with
       | Sandbox.OK -> record_ok u progress comp sol
       | Sandbox.Failed l -> record_failed u progress comp l
       end
    end;
    output_results progress;
  end

let retries = ref 5
let seed = ref 123
let compilers = ref []

let print_version () =
  printf "2.0.0\n";
  exit 0

let spec = [
  "-retries", Arg.Set_int retries,
           "<n> retry failed packages <n> times (default 5)";
  "-seed", Arg.Set_int seed, "<n> set pseudo-random seed to <n>";
  "-version", Arg.Unit print_version, " print version number and exit";
]

let usage = "usage: opamcheck [-retries <n>] [-seed <n>] version..."

let main () =
  Arg.parse spec (fun s -> compilers := s :: !compilers) usage;
  if !compilers = [] then begin
    Arg.usage spec usage;
    exit 1;
  end;
  Random.init !seed;
  let excludes = read_lines (Filename.concat sandbox "exclude") in
  let f accu dir name = parse_file dir name :: accu in
  let asts = fold_opam_files f [] repo in
  Status.(cur.pack_total <- List.length asts);
  let (u, packs) = Package.make !compilers asts in
  (* List.iter (Package.show u) packs; *)
  let progress = { statuses = STM.empty; num_done = 0; num_ok = 0 } in
  (* TODO refaire cette boucle avec deux fonctions *)
  let rec loop comp comps packs i =
    if i >= !retries then begin
      match comps with
      | [] -> ()
      | h :: t ->
         output_results progress;
         loop h t packs 0
    end else begin
      List.iter (test_comp_pack (i = 0) u excludes packs progress comp) packs;
      let filt p =
        match
          STM.find (comp, p.Package.name, p.Package.version) progress.statuses
        with
        | { finished = true; _ } -> false
        | _ -> true
        | exception Not_found -> true
      in
      loop comp comps (List.filter filt packs) (i + 1)
    end
  in
  match !compilers with
  | [] -> Arg.usage spec usage; exit 1
  | comp :: comps -> loop comp comps packs 0

;; Printexc.catch main ()
