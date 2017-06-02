(* main.ml -- main program for opamcheck
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

open Util

let parse_opam file lb =
  Parsing_aux.file := file;
  Parsing_aux.line := 1;
  try Parser.opam Lexer.token lb
  with
  | Parser.Error ->
     eprintf "\"%s\":%d -- syntax error\n" file !Parsing_aux.line; exit 2
  | Failure msg ->
     eprintf "\"%s\":%d -- lexer error: %s\n" file !Parsing_aux.line msg;
     exit 2

let parse_file dir file =
  Status.(cur.step <- Read file; show ());
  let ic = open_in file in
  let lb = Lexing.from_channel ic in
  let res =
    try
      let res = parse_opam file lb in
      Status.show_result '+';
      res
    with _ -> Status.show_result '#'; []
  in
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

type status =
  | Fail of int
  | OK
  | Uninst

let read_lines file =
  if Sys.file_exists file then begin
    let ic = open_in file in
    let rec loop set =
      match input_line ic with
      | s -> loop (s :: set)
      | exception End_of_file -> set
    in
    let result = loop [] in
    close_in ic;
    result
  end else
    []

let cache = ref (SPLS.singleton [])
let sat = Minisat.create ();

type progress = {
  mutable statuses : (string * status) list SPM.t;
  mutable num_done : int;
  mutable num_ok : int;
}

let get_status p name vers comp =
  try
    snd (List.find (fun (c, _) -> c = comp) (SPM.find (name, vers) p.statuses))
  with Not_found -> Fail 0

let set_status p name vers comp st =
  let l =
    try
      let l = SPM.find (name, vers) p.statuses in
      let f (c, _) = c <> comp in
      (comp, st) :: List.filter f l
    with Not_found -> [(comp, st)]
  in
  p.statuses <- SPM.add (name, vers) l p.statuses

let print_solution chan l =
  fprintf chan "[";
  List.iter (fun (n, v) -> fprintf chan " %s.%s" n v) l;
  fprintf chan " ]"

let results = open_out (Filename.concat sandbox "results")

let record_ok u p comp l =
  eprintf "ok: "; print_solution stderr l; eprintf "\n"; flush stderr;
  let (tag, list) = Sandbox.get_tag l in
  fprintf results "ok %s [%s ]\n" tag list; flush results;
  let add_ok (name, vers) =
    let st = get_status p name vers comp in
    if st <> OK then begin
      set_status p name vers comp OK;
      p.num_done <- p.num_done + 1;
      p.num_ok <- p.num_ok + 1;
    end
  in
  let rec loop l =
    cache := SPLS.add l !cache;
    match l with
    | [] -> ()
    | h :: t -> add_ok h; loop t
  in
  loop l

let forbid_solution u l =
  let f (n, v) = Minisat.Lit.neg (Package.find_lit u n v) in
  Minisat.add_clause_l u.Package.sat (List.map f l)

let record_failed u p comp l =
  eprintf "failed: "; print_solution stderr l; eprintf "\n"; flush stderr;
  let (tag, list) = Sandbox.get_tag l in
  fprintf results "fail %s [%s ]\n" tag list; flush results;
  forbid_solution u l;
  match l with
  | [] -> assert false
  | (name, vers) :: t ->
     begin match get_status p comp name vers with
     | OK -> ()
     | Fail n -> set_status p comp name vers (Fail (n + 1))
     | Uninst -> assert false
     end;
     record_ok u p comp t

let record_uninst u p comp name vers =
  eprintf "uninst: %s.%s\n" name vers; flush stderr;
  fprintf results "uninst %s.%s\n" name vers; flush results;
  match get_status p name vers comp with
  | Uninst -> ()
  | Fail 0 ->
     p.num_done <- p.num_done + 1;
     set_status p name vers comp Uninst
  | OK | Fail _ -> assert false

let find_sol u comp name vers =
  let result = ref None in
  let n = ref 0 in
  let check prev =
    incr n;
    Status.(cur.step <- Solve !n; show ());
    match Solver.solve u prev ~ocaml:comp ~pack:name ~vers with
    | None -> ()
    | Some raw_sol ->
       let sol = List.filter Env.is_package raw_sol in
       begin try
         result := Some (Solver.schedule u prev sol);
         raise Exit
       with Solver.Schedule_failure (partial, remain) ->
         eprintf "Warning: schedule failed, partial = ";
         print_solution stderr partial;
         eprintf "\nremain = ";
         print_solution stderr remain;
         eprintf "\n";
         flush stderr;
         forbid_solution u raw_sol;
       end
  in
  (try SPLS.iter check !cache with Exit -> ());
  begin match !result with
  | None -> Status.show_result '#'
  | Some _ -> Status.show_result '+'
  end;
  !result

let test_comp_pack u progress comp pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
eprintf "testing: %s.%s\n" name vers; flush stderr;
  match get_status progress name vers comp with
  | OK | Uninst ->
     eprintf "testing: %s.%s OK/Uninst\n" name vers; flush stderr;
  | Fail n ->
    begin
      Status.(
        cur.ocaml <- comp;
        cur.pack_cur <- sprintf "%s.%s" name vers;
        cur.pack_ok <- progress.num_ok;
        cur.pack_done <- progress.num_done;
      );
      match find_sol u comp name vers with
      | None ->
         eprintf "testing: %s.%s no solution\n" name vers; flush stderr;
         record_uninst u progress comp name vers
      | Some sched ->
         eprintf "testing: %s.%s solution: " name vers;
         print_solution stderr sched;
         eprintf "\n"; flush stderr;
         match Sandbox.play_solution sched with
         | Sandbox.OK -> record_ok u progress comp sched
         | Sandbox.Failed l -> record_failed u progress comp l
    end

let register_exclusion u s =
  let (name, vers) = Version.split_name_version s in
  try
    match vers with
    | Some v -> forbid_solution u [(name, v)]
    | None ->
       let f (v, _) = forbid_solution u [(name, v)] in
       List.iter f (SM.find name u.Package.lits)
  with Not_found ->
    eprintf "Warning in excludes: %s not found\n" s; flush stderr

let do_package u p comp comps pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  let is_ok c = get_status p name vers c = OK in
  if get_status p name vers comp = Fail 0 || List.exists is_ok comps then
    test_comp_pack u p comp pack
  else
    let f (best, best_n) c =
      match get_status p name vers c with
      | Fail n -> if n <= best_n then (c, n) else (best, best_n)
      | OK | Uninst -> assert false
    in
    let (best, _) = List.fold_left f (comp, max_int) (comp :: comps) in
    test_comp_pack u p best pack

let retries = ref 5
let seed = ref 123
let compilers = ref []

let unfinished p comp pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  match get_status p name vers comp with
  | OK | Uninst -> false
  | Fail n -> n < !retries

let print_version () =
  printf "2.1.0\n";
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
  let f accu dir name = parse_file dir name :: accu in
  let asts = fold_opam_files f [] repo in
  Status.(cur.pack_total <- List.length asts);
  let u = Package.make !compilers asts in
  let excludes = read_lines (Filename.concat sandbox "exclude") in
  List.iter (register_exclusion u) excludes;
  let p = { statuses = SPM.empty; num_done = 0; num_ok = 0 } in

  let loop_limit = !retries * List.length !compilers in
  let rec loop comp comps packs i =
    if i >= loop_limit then () else begin
      List.iter (do_package u p comp comps) packs;
      let packs = List.filter (unfinished p comp) packs in
      loop comp comps packs (i + 1)
    end
  in
  match !compilers with
  | [] -> Arg.usage spec usage; exit 1
  | comp :: comps -> loop comp comps u.Package.packs 0

;; Printexc.catch main ()
