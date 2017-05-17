(* main.ml -- main program for opamcheck
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

let parse_opam lb =
  Lexer.line := 1;
  try Parser.opam Lexer.token lb
  with
  | Parser.Error -> eprintf "parse error at line %d\n" !Lexer.line; exit 2
  | Failure msg ->
     eprintf "lexer error at line %d: %s\n" !Lexer.line msg;
     exit 2

let parse_file dir file =
  let ic = open_in file in
  let lb = Lexing.from_channel ic in
  let res = try parse_opam lb with _ -> [] in
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

let print_solution l =
  let print_pack (pack, vers) =
    if vers <> "." then printf " %s.%s" pack vers
  in
  List.iter print_pack l;
  printf "\n"

let sandbox = Sys.getenv "OPCSANDBOX"

let repo = Filename.concat sandbox "opam-repository"

type status = {
  mutable finished : bool;
  mutable ok : int;
  mutable failed : int;
  mutable forbid : Vdd.t;
}

module StringTriple = struct
  type t = string * string * string
  let compare = Pervasives.compare
end

module STM = Map.Make (StringTriple)

let make_failure u l =
  let f v (name, vers) = Vdd.mk_or u v (Vdd.atom u name ((<>) vers)) in
  List.fold_left f (Vdd.mk_false u) l

let get_status u statuses comp name vers =
  match STM.find (comp, name, vers) !statuses with
  | r -> r
  | exception Not_found ->
     let r = { finished = false; ok = 0; failed = 0; forbid = Vdd.mk_true u } in
     statuses := STM.add (comp, name, vers) r !statuses;
     r

let record_finished u statuses comp name vers =
  printf "DONE: %s / %s.%s\n" comp name vers; flush stdout;
  let r = get_status u statuses comp name vers in
  r.finished <- true

let record_ok u statuses comp l =
  let add_ok (name, vers) =
    let r = get_status u statuses comp name vers in
    if not r.finished then
      printf "OK: %s / %s.%s\n" comp name vers; flush stdout;
    r.finished <- true;
    r.ok <- r.ok + 1;
  in
  List.iter add_ok l

let record_failed u statuses comp l =
  match l with
  | [] -> assert false
  | (name, vers) :: t ->
     let r = get_status u statuses comp name vers in
     r.failed <- r.failed + 1;
     r.forbid <- Vdd.mk_and u r.forbid (make_failure u l);
     if r.ok = 0 then
       printf "FAIL: %s / %s.%s (%d)\n" comp name vers r.failed; flush stdout;
     record_ok u statuses comp t

let stchan = open_out (Filename.concat (Sys.getenv "OPCSANDBOX") "status")
let output_status statuses =
  let pr (comp, name, vers) r =
    let desc =
      if r.ok > 0 then "OK"
      else if r.finished && r.failed = 0 then "unavailable"
      else if r.finished then "FAIL"
      else sprintf "try(%d)" r.failed
    in
    fprintf stchan "%s / %s.%s : %s\n" comp name vers desc
  in
  STM.iter pr !statuses;
  flush stchan

let print_schedule_failure sol remains =
  fprintf stchan "CANNOT SCHEDULE:";
  List.iter (fun (n, v) -> fprintf stchan " %s.%s" n v) sol;
  fprintf stchan "\n";
  fprintf stchan "REMAINS :";
  List.iter (fun (n, v) -> fprintf stchan " %s.%s" n v) remains;
  fprintf stchan "\n";
  flush stchan

let test_comp_pack first u packs statuses comp pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  printf "Checking package %s.%s with %s\n" name vers comp; flush stdout;
  if not (get_status u statuses comp name vers).finished then begin
    printf "Solving...\n"; flush stdout;
    let (sols, l) = Solver.solve u packs ~ocaml:comp ~pack:name ~vers in
    let r = get_status u statuses comp name vers in
    let sols = Vdd.mk_and u sols r.forbid in
    if Vdd.is_false u sols then begin
      printf "unavailable\n"; flush stdout;
      record_finished u statuses comp name vers
    end else begin
      printf "Counting...\n"; flush stdout;
      let ct = Vdd.count u sols in
      let n = Vdd.get_count u ct in
      assert (n > 0);
      let r = if first then 0 else Random.int (min 0x3FFFFFFF n) in
      fprintf stchan "%s / %s.%s : trying solution %d of %d:"
        comp name vers r n;
      let sol = Vdd.get_nth u ct r in
      List.iter (fun (n, v) -> fprintf stchan " %s.%s" n v) sol;
      fprintf stchan "\n";
      flush stchan;
      match Solver.schedule u packs sol with
      | sched ->
         if Sys.file_exists (Filename.concat sandbox "stop") then
           Pervasives.exit 10;
         begin match Sandbox.play_solution comp sched with
         | Sandbox.OK -> record_ok u statuses comp sched
         | Sandbox.Failed l -> record_failed u statuses comp l
         end
      | exception (Solver.Schedule_failure remains as e) ->
         print_schedule_failure sol remains;
         raise e
    end;
    output_status statuses;
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
  let f accu dir name = parse_file dir name :: accu in
  let asts = fold_opam_files f [] repo in
  let (u, packs) = Package.make !compilers asts in
  (* List.iter (Package.show u) packs; *)
  let status = ref STM.empty in
  (* TODO refaire cette boucle avec deux fonctions *)
  let rec loop comp comps packs i =
    printf "testing %d packages with %s (pass %d)\n"
      (List.length packs) comp i;
    flush stdout;
    if i >= !retries then begin
      match comps with
      | [] -> ()
      | h :: t ->
         output_status status;
         loop h t packs 0
    end else begin
      List.iter (test_comp_pack (i = 0) u packs status comp) packs;
      let filt p =
        match STM.find (comp, p.Package.name, p.Package.version) !status with
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
