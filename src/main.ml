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
}

module StringTriple = struct
  type t = string * string * string
  let compare = Pervasives.compare
end

module STM = Map.Make (StringTriple)

let get_status statuses comp name vers =
  match STM.find (comp, name, vers) !statuses with
  | r -> r
  | exception Not_found ->
     let r = { finished = false; ok = 0; failed = 0 } in
     statuses := STM.add (comp, name, vers) r !statuses;
     r

let record_finished statuses comp name vers =
  printf "DONE: %s / %s.%s" comp name vers;
  let r = get_status statuses comp name vers in
  r.finished <- true

let record_ok statuses comp l =
  let add_ok (name, vers) =
    printf "OK: %s / %s.%s" comp name vers;
    let r = get_status statuses comp name vers in
    r.finished <- true;
    r.ok <- r.ok + 1;
  in
  List.iter add_ok l

let record_failed statuses comp l =
  match l with
  | [] -> assert false
  | (name, vers) :: t ->
     printf "FAIL: %s / %s.%s" comp name vers;
     let r = get_status statuses comp name vers in
     r.failed <- r.failed + 1;
     record_ok statuses comp t

let stchan = open_out (Filename.concat (Sys.getenv "OPCSANDBOX") "status")
let output_status statuses =
  let pr (comp, name, vers) r =
    let desc =
      if r.ok > 0 then "OK"
      else if r.finished && r.failed = 0 then "uninstallable"
      else if r.finished then "FAIL"
      else sprintf "try(%d)" r.failed
    in
    fprintf stchan "%s / %s.%s : %s\n" comp name vers desc
  in
  STM.iter pr !statuses;
  flush stchan

let make_failure u l =
  let f v (name, vers) = Vdd.mk_or u v (Vdd.atom u name ((<>) vers)) in
  List.fold_left f (Vdd.mk_false u) l

let test_comp_pack u packs failures statuses comp pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  if not (get_status statuses comp name vers).finished then begin
    printf "Testing package %s.%s with %s\n" name vers comp;
    let (sols, l) = Solver.solve u packs ~ocaml:comp ~pack:name ~vers in
    let sols = Vdd.mk_and u sols !failures in
    let ct = Vdd.count u sols in
    let n = Vdd.get_count u ct in
    if n = 0 then
      record_finished statuses comp name vers
    else begin
      let r = Random.int (min 0x3FFFFFFF n) in
      fprintf stchan "%s / %s.%s : trying solution %d of %d\n"
        comp name vers r n;
      flush stchan;
      let sol = Vdd.get_nth u ct r in
      let sched = Solver.schedule u packs sol in
      match Sandbox.play_solution comp sched with
      | Sandbox.OK -> record_ok statuses comp sched
      | Sandbox.Failed l ->
         record_failed statuses comp l;
         failures := Vdd.mk_and u !failures (make_failure u l);
    end
  end;
  output_status statuses

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
  let failures = ref (Vdd.mk_true u) in
  let status = ref STM.empty in
  let rec loop comp comps packs i =
    if i >= !retries then begin
      match comps with
      | [] -> ()
      | h :: t ->
         output_status status;
         loop h t packs 0
    end else begin
      List.iter (test_comp_pack u packs failures status comp) packs;
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
