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

let sandbox = Filename.concat (Sys.getenv "OPAMCHECKDIR") "sandbox"

let repo = Filename.concat sandbox "opam-repository"

let stchan = open_out (Filename.concat sandbox "status")

let output_failure comp name vers =
  fprintf stchan "%s.%s %s:FAIL\n" name vers comp;
  flush stchan

let output_try comp name vers i =
  fprintf stchan "%s.%s %s:try[%d]\n" name vers comp i;
  flush stchan

let output_ok comp name vers =
  fprintf stchan "%s.%s %s:OK\n" name vers comp;
  flush stchan

let make_failure u l =
  let f v (name, vers) = Vdd.mk_or u v (Vdd.atom u name ((<>) vers)) in
  List.fold_left f (Vdd.mk_true u) l

let test_pack_comp u packs failures pack comp =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  eprintf "Testing package %s.%s with %s\n" name vers comp;
  flush stderr;
  let (sols, l) = Solver.solve u packs ~ocaml:comp ~pack:name ~vers in
  let rec loop sols i =
    if i > 10 then false else begin
      let ct = Vdd.count u sols in
      let n = Vdd.get_count u ct in
      if n = 0 then begin
        output_failure comp name vers;
        false
      end else begin
        let sol = Vdd.get_nth u ct (Random.int n) in
        let sched = Solver.schedule u packs sol in
        output_try comp name vers i;
        match Sandbox.play_solution comp sched with
        | Sandbox.OK -> output_ok comp name vers; true
        | Sandbox.Failed l ->
           let condition = make_failure u l in
           failures := Vdd.mk_and u !failures condition;
           loop (Vdd.mk_and u sols condition) (i + 1)
      end
    end
  in
  loop sols 0

let rec test_pack u packs failures compilers pack =
  ignore (List.exists (test_pack_comp u packs failures pack) compilers)

let seed = ref 123
let compilers = ref []

let print_version () =
  printf "2.0.0\n";
  exit 0

let spec = [
  "-seed", Arg.Set_int seed, " set pseudo-random seed"
  "-version", Arg.Unit print_version, " print version number and exit"
]

let usage = "usage: opamcheck [-seed <n>] version..."

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
  List.iter (test_pack u packs failures !compilers) packs

;; main ()
