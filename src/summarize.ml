(* summarize.ml -- display opamcheck results in HTML
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

open Util

let show_ok = ref false
let show_uninst = ref false
let version = ref ""

type status = OK | Uninst | Fail | Unknown

let get m p =
  try SM.find p m with Not_found -> (Unknown, Unknown)

let merge x y =
  match x, y with
  | OK, _ | _, OK -> OK
  | Uninst, _ | _, Uninst -> Uninst
  | Fail, _ | _, Fail -> Fail
  | Unknown, Unknown -> Unknown

let add status comp m p =
  assert (String.sub comp 0 9 = "compiler.");
  let comp = String.sub comp 9 (String.length comp - 9) in
  let (st_old, st_new) = get m p in
  let st =
    if comp = !version then begin
      (st_old, merge st_new status)
    end else begin
      (merge st_old status, st_new)
    end
  in
  SM.add p st m

let rec parse_list l accu =
  match l with
  | [] -> failwith "missing close bracket"
  | [ comp; "]" ] -> comp, List.rev (comp :: accu)
  | h :: t -> parse_list t (h :: accu)

let parse_line s m =
  let words = String.split_on_char ' ' s in
  match words with
  | "ok" :: tag :: "[" :: l ->
     let (comp, l) = parse_list l [] in
     List.fold_left (add OK comp) m l
  | ["uninst"; pack; comp] ->
     add Uninst ("compiler." ^ comp) m pack
  | "fail" :: tag :: "[" :: pack :: l ->
     let (comp, l) = parse_list l [] in
     let m = add Fail comp m pack in
     List.fold_left (add OK comp) m l
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
  | _, OK -> "ok"
  | OK, Uninst -> "new_uninst"
  | _, Uninst -> "old_uninst"
  | Fail, Fail -> "old_fail"
  | Unknown, Fail -> "unk_fail"
  | _, Fail -> "new_fail"
  | _, Unknown -> "unknown"

let print_result (p, st) =
  let (_, vers) = Version.split_name_version p in
  match vers with
  | None -> failwith "missing version number in results"
  | Some vers ->
     printf "  <span class=\"%s\">%s</span>\n" (color st) vers

let print_result_line l =
  match l with
  | [] -> assert false
  | (p, _) :: _ ->
     let (name, _) = Version.split_name_version p in
     printf "<div> %s\n" name;
     List.iter print_result l;
     printf "</div>\n"

let spec = Arg.[
  "-ok", Set show_ok, " Show OK results";
  "-uninst", Set show_uninst, " Show Uninst results";
]

let anon v =
  if !version <> "" then raise (Arg.Bad "too many arguments");
  version := v

let usage = "usage: summarize [-ok] [-uninst] <version> <results"

let html_header = "\
<!DOCTYPE html>\n\
<html><head>\n\
<style>\n\
.ok {background-color: #99ff99;}\n\
.new_uninst {background-color: #ffff30;}\n\
.old_uninst {background-color: #cccccc;}\n\
.new_fail {background-color: #ff3030;}\n\
.old_fail {background-color: #eb99ff;}\n\
.unk_fail {background-color: #ffcccc;}\n\
.unkown {background-color: white;}\n\
</style>\n\
</head>\n\
<body>\n\
"

let html_footer = "\
</body></html>\n\
"

let main () =
  Arg.parse spec anon usage;
  if !version = "" then (Arg.usage spec usage; exit 2);
  let results = SM.bindings (parse stdin) in
  let groups = group_packs results [] in
  print_string html_header;
  List.iter print_result_line groups;
  print_string html_footer

;; Printexc.catch main ()
