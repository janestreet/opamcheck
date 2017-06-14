(* main.ml -- main program for opamcheck
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

open Util

let retries = ref 5
let seed = ref 123
let compilers = ref []

let parse_opam file lb =
  Parsing_aux.file := file;
  Parsing_aux.line := 1;
  try Parser.opam Lexer.token lb
  with
  | Parser.Error ->
     Log.fatal "\"%s\":%d -- syntax error\n" file !Parsing_aux.line
  | Failure msg ->
     Log.fatal "\"%s\":%d -- lexer error: %s\n" file !Parsing_aux.line msg

let parse_file dir file =
  Status.(cur.step <- Read Filename.(basename (dirname file)); show ());
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

let repo = Filename.concat Util.sandbox "opam-repository"

type status =
  | Try of int * int  (* number of fails, number of depfails *)
  | OK
  | Uninst
  | Fail

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
  mutable num_ok : int;
  mutable num_uninst : int;
  mutable num_fail : int;
}

let get_status p name vers comp =
  try
    snd (List.find (fun (c, _) -> c = comp) (SPM.find (name, vers) p.statuses))
  with Not_found -> Try (0, 0)

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

let record_ok u p comp l =
  let (tag, list) = Sandbox.get_tag l in
  Log.res "ok %s [%s ]\n" tag list;
  let add_ok (name, vers) =
    match get_status p name vers comp with
    | OK -> ()
    | Try _ ->
       set_status p name vers comp OK;
       p.num_ok <- p.num_ok + 1;
    | Fail | Uninst -> assert false
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
  let (tag, list) = Sandbox.get_tag l in
  Log.res "fail %s [%s ]\n" tag list;
  forbid_solution u l;
  match l with
  | [] -> assert false
  | (name, vers) :: t ->
     begin match get_status p name vers comp with
     | OK -> ()
     | Try (f, d) ->
        if f >= !retries then begin
          forbid_solution u [("compiler", comp); (name, vers)];
          p.num_fail <- p.num_fail + 1;
          set_status p name vers comp Fail
        end else begin
          set_status p name vers comp (Try (f + 1, d))
        end
     | Uninst | Fail -> assert false
     end;
     record_ok u p comp t

let record_uninst u p comp name vers =
  Log.res "uninst %s.%s %s\n" name vers comp;
  match get_status p name vers comp with
  | Try (0, 0) ->
     p.num_uninst <- p.num_uninst + 1;
     set_status p name vers comp Uninst
  | _ -> assert false

let record_depfail u p comp name vers l =
  match get_status p name vers comp with
  | OK -> ()
  | Try (f, d) ->
     let (tag, list) = Sandbox.get_tag l in
     Log.res "depfail %s %s.%s [%s ]\n" tag name vers list;
     set_status p name vers comp (Try (f, d + 1))
  | Uninst | Fail -> assert false

let randomize () =
  let seed = Random.bits () in
  fun x y ->
    let hx = Hashtbl.hash x in
    let hy = Hashtbl.hash y in
    let dx = Digest.string (sprintf "%d %d" seed hx) in
    let dy = Digest.string (sprintf "%d %d" seed hy) in
    compare dx dy

let find_sol u comp name vers attempt =
  let result = ref None in
  let n = ref 0 in
  let check prev =
    incr n;
    Status.(cur.step <- Solve (!n, List.length prev));
    match Solver.solve u prev ~ocaml:comp ~pack:name ~vers with
    | None -> ()
    | Some raw_sol ->
       let sol = List.filter Env.is_package raw_sol in
       begin try
         result := Some (Solver.schedule u prev sol);
         raise Exit
       with Solver.Schedule_failure (partial, remain) ->
         Log.warn "schedule failed, partial = ";
         print_solution Log.warn_chan partial;
         Log.warn "\nremain = ";
         print_solution Log.warn_chan remain;
         Log.warn "\n";
         forbid_solution u raw_sol;
       end
  in
  (* Look for a solution in an empty environment before trying to solve
     with cached states. If there is none, the package is uninstallable. *)
  Status.(cur.step <- Solve (0, 0));
  let empty_sol = Solver.solve u [] ~ocaml:comp ~pack:name ~vers in
  if empty_sol = None then begin
    result := None
  end else begin
    (* On first attempt, use cache in largest-first order; on second
       attempt use empty cache; on later attempts use randomized cache. *)
    let cached =
      match attempt with
      | 0 -> SPLS.elements !cache
      | 1 -> [ Sandbox.ask_opam comp name vers ]
      | _ -> List.sort (randomize ()) (SPLS.elements !cache)
    in
    (try List.iter check cached with Exit -> ());
  end;
  Status.show ();
  Status.show_result (if !result = None then '#' else '+');
  !result

let test_comp_pack u progress comp pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  let st = get_status progress name vers comp in
  if st <> OK then begin
    Log.log "testing: %s.%s\n" name vers;
    Status.(
      cur.ocaml <- comp;
      cur.pack_cur <- sprintf "%s.%s" name vers;
    );
    let attempt =
      match st with
      | Try (f, d) -> f + d
      | _ -> 2
    in
    match find_sol u comp name vers attempt with
    | None ->
       Log.log "no solution\n"
    | Some sched ->
       Log.log "solution: ";
       print_solution Log.log_chan sched;
       Log.log "\n";
       match Sandbox.play_solution sched with
       | Sandbox.OK -> record_ok u progress comp sched
       | Sandbox.Failed l ->
          record_failed u progress comp l;
          if List.hd l <> (name, vers) then
            record_depfail u progress comp name vers l
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
    Log.warn "Warning in excludes: %s not found\n" s

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
  let u = Package.make !compilers asts in
  let excludes = read_lines (Filename.concat sandbox "exclude") in
  List.iter (register_exclusion u) excludes;
  let p = {
    statuses = SPM.empty;
    num_ok = 0;
    num_uninst = 0;
    num_fail = 0;
  } in
  let comp, comps =
    match !compilers with
    | [] -> Arg.usage spec usage; exit 1
    | comp :: comps -> (comp, comps)
  in
  let cmp p1 p2 =
    Package.(
      let c = Pervasives.compare p1.name p2.name in
      if c = 0 then Version.compare p2.version p1.version else c
    )
  in
  let packs = List.sort cmp u.Package.packs in
  (* Start by recording truly uninstallable packages. Anything that
     becomes uninstallable after that, is in fact a depfail.
  *)
  let check_inst comp pack =
    let name = pack.Package.name in
    let vers = pack.Package.version in
    match Solver.solve u [] ~ocaml:comp ~pack:name ~vers with
    | None -> record_uninst u p comp name vers
    | Some _ -> ()
  in
  Status.(cur.step <- Solve (0, 0); show ());
  List.iter (fun comp -> List.iter (check_inst comp) packs) !compilers;
  let is_done c pack fail_done =
    match get_status p pack.Package.name pack.Package.version c with
    | OK | Uninst -> true
    | Try _ -> false
    | Fail -> fail_done
  in
  (* First pass: try each package twice with latest compiler. *)
  let packs = List.filter (fun p -> not (is_done comp p false)) packs in
  Status.(
    cur.pass <- 1;
    cur.pack_done <- 0;
    cur.pack_total <- List.length packs
  );
  let f pack =
    test_comp_pack u p comp pack;
    if not (is_done comp pack false) then test_comp_pack u p comp pack;
    Status.(cur.pack_done <- cur.pack_done + 1)
  in
  List.iter f packs;
  (* Second pass: try failing packages with every other compiler
     twice: once with cache and once without. *)
  let packs = List.filter (fun p -> not (is_done comp p false)) packs in
  Status.(
    cur.pass <- 2;
    cur.pack_done <- 0;
    cur.pack_total <- List.length packs
  );
  let f pack =
    let rec loop comps =
      match comps with
      | [] -> ()
      | h :: t ->
         test_comp_pack u p h pack;
         if not (is_done h pack true) then test_comp_pack u p h pack;
         if not (is_done h pack true) then loop t
    in
    loop comps;
    Status.(cur.pack_done <- cur.pack_done + 1)
  in
  List.iter f packs;
  (* Third pass: try newly-failing packages [retries] times. *)
  let is_ok c pack =
    get_status p pack.Package.name pack.Package.version c = OK
  in
  let is_new_fail pack = List.exists (fun c -> is_ok c pack) comps in
  let packs = List.filter is_new_fail packs in
  Status.(
    cur.pass <- 3;
    cur.pack_done <- 0;
    cur.pack_total <- List.length packs
  );
  let f pack =
    let rec loop i =
      if i > 0 && not (is_done comp pack true) then begin
        test_comp_pack u p comp pack;
        loop (i - 1)
      end
    in
    loop !retries;
    Status.(cur.pack_done <- cur.pack_done + 1)
  in
  List.iter f packs;
  Status.message "\nDONE\n"

;; Printexc.catch main ()
