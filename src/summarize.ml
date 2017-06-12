(* summarize.ml -- display opamcheck results in HTML
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

open Util

let show_all = ref false
let version = ref ""

type status = OK | Uninst | Fail | Depfail | Unknown

let get m p =
  try SM.find p m with Not_found -> (Unknown, Unknown, [])

let merge x y =
  match x, y with
  | OK, _ | _, OK -> OK
  | Fail, _ | _, Fail -> Fail
  | Depfail, _ | _, Depfail -> Depfail
  | Uninst, _ | _, Uninst -> Uninst
  | Unknown, Unknown -> Unknown

let add status line comp m p =
  assert (String.sub comp 0 9 = "compiler.");
  let comp = String.sub comp 9 (String.length comp - 9) in
  let (st_old, st_new, lines) = get m p in
  let lines = if List.mem line lines then lines else line :: lines in
  let st =
    if comp = !version then begin
      (st_old, merge st_new status, lines)
    end else begin
      (merge st_old status, st_new, lines)
    end
  in
  SM.add p st m

let rec find_comp l =
  match l with
  | [] -> failwith "missing close bracket"
  | [ comp; "]" ] -> comp
  | h :: t -> find_comp t

let parse_list l =
  match l with
  | [] -> failwith "missing close bracket"
  | h :: t -> (find_comp l, h)

let parse_line s m =
  let words = String.split_on_char ' ' s in
  match words with
  | "ok" :: tag :: "[" :: l ->
     let (comp, pack) = parse_list l in
     add OK s comp m pack
  | ["uninst"; pack; comp] ->
     add Uninst s ("compiler." ^ comp) m pack
  | "depfail" :: tag :: pack :: "[" :: l ->
     let (comp, _) = parse_list l in
     add Depfail s ("compiler." ^ comp) m pack
  | "fail" :: tag :: "[" :: l ->
     let (comp, pack) = parse_list l in
     add Fail s comp m pack
  | _ -> failwith "syntax error in results file"

let parse chan =
  let rec loop m =
    match input_line chan with
    | l -> loop (parse_line l m)
    | exception End_of_file -> m
  in
  loop SM.empty

let same_pack p1 p2 =
  let (name1, _) = Version.split_name_version p1 in
  let (name2, _) = Version.split_name_version p2 in
  name1 = name2

let rec group_packs l accu =
  match l with
  | [] -> List.rev accu
  | (pack, _) as h :: t -> group_packs_with pack t [h] accu
and group_packs_with p l accu1 accu2 =
  match l with
  | (pack, _) as h :: t when same_pack p pack ->
     group_packs_with p t (h :: accu1) accu2
  | _ -> group_packs l (accu1 :: accu2)

let color status =
  match status with
  | _, OK, _ -> ("ok", "o")
  | OK, Uninst, _ -> ("new_uninst", "U")
  | _, Uninst, _ -> ("uninst", "u")
  | OK, Fail, _ -> ("new_fail", "X")
  | Fail, Fail, _ -> ("old_fail", "x")
  | _, Fail, _ -> ("fail", "x")
  | OK, Depfail, _ -> ("new_depfail", "D")
  | _, Depfail, _ -> ("depfail", "d")
  | _, Unknown, _ -> ("unknown", "?")

let print_details file (_, _, lines) =
  let oc = open_out (Filename.concat "results.dir" file) in
  List.iter (fprintf oc "%s\n\n") lines;
  close_out oc

let print_result oc (p, st) =
  let (_, vers) = Version.split_name_version p in
  match vers with
  | None -> failwith "missing version number in results"
  | Some vers ->
     let auxfile = p ^ ".txt" in
     let (col, txt) = color st in
     fprintf oc "  <td class=\"%s\"><div class=\"tt\"><a href=\"%s\">%s\
                     </a><span class=\"ttt\">%s %s</span></div></td>\n"
       col auxfile txt vers col;
     print_details auxfile st

let compare_vers (p1, _) (p2, _) =
  match (Version.split_name_version p1, Version.split_name_version p2) with
  | (_, Some v1), (_, Some v2) -> Version.compare v2 v1
  | _ -> assert false

let is_interesting l =
  let f (pack, st) =
    fst (Version.split_name_version pack) <> "compiler"
    && match color st with
       | ("ok" | "uninst" | "new_uninst"), _ -> !show_all
       | _ -> true
  in
  List.exists f l

let print_result_line oc l =
  match l with
  | [] -> assert false
  | (p, _) :: _ ->
     if is_interesting l then begin
       let (name, _) = Version.split_name_version p in
       fprintf oc "<tr><th>%s</th>\n" name;
       List.iter (print_result oc) (List.sort compare_vers l);
       fprintf oc "</tr>\n"
     end

let spec = Arg.[
  "-all", Set show_all, " Show all results";
]

let anon v =
  if !version <> "" then raise (Arg.Bad "too many arguments");
  version := v

let usage = "usage: summarize [-all] <version> <results"

let html_header = "\
<!DOCTYPE html>\n\
<html><head>\n\
<style>\n\
.ok {background-color: #66ff66;}\n\
.new_uninst {background-color: #ffff30;}\n\
.uninst {background-color: #cccccc;}\n\
.new_depfail {background-color: #ff8800;}\n\
.depfail {background-color: #ffe0cc;}\n\
.new_fail {background-color: #ff3030;}\n\
.old_fail {background-color: #eb99ff;}\n\
.fail {background-color: #ffcccc;}\n\
.unknown {background-color: #bbbbff;}\n\
.tt {\n\
    position: relative;\n\
    display: inline-block;\n\
}\n\
.tt .ttt {\n\
    visibility: hidden;\n\
    width: 120px;\n\
    background-color: #ffeedd;\n\
    text-align: center;\n\
    padding: 5px 5px;\n\
    position: absolute;\n\
    z-index: 1;\n\
    top: 120%;\n\
    left: 50%;\n\
    margin-left: -60px;\n\
}\n\
.tt:hover .ttt { visibility: visible; }\n\
th { text-align: right; }\n\
td { text-align: center; }\n\
</style>\n\
</head>\n\
<body>\n\
<table>\n\
"

let html_footer = "</table></body></html>\n"

let main () =
  Arg.parse spec anon usage;
  if !version = "" then (Arg.usage spec usage; exit 2);
  let results = SM.bindings (parse stdin) in
  let groups = group_packs results [] in
  ignore (Sys.command "mkdir -p results.dir");
  let index = open_out (Filename.concat "results.dir" "index.html") in
  fprintf index "%s" html_header;
  List.iter (print_result_line index) groups;
  fprintf index "%s" html_footer

;; Printexc.catch main ()
