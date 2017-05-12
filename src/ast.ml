(* ast.ml -- abstract syntax tree for OPAM's opam and url files
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

type opam =
  | Name of string
  | Version of string
  | Depends of package formula
  | Depopts of package formula
  | Conflicts of package list
  | Available of filter formula
  | Ocaml_version of constrain formula
  | Skip

and 'a formula =
  | And of 'a formula * 'a formula
  | List of 'a formula list
  | Or of 'a formula * 'a formula
  | Not of 'a formula
  | Atom of 'a

and package = string * constrain formula option

and filter = string * constrain option

and comp = Eq | Lt | Gt | Le | Ge | Ne

and constrain = comp * string

type url =
  | Mirrors of string list
  | Key of string * string


let show_list name f l =
  printf "%s = [\n" name;
  List.iter f l;
  printf "]\n"

let show_comp = function
  | Eq -> printf "="
  | Lt -> printf "<"
  | Gt -> printf ">"
  | Le -> printf "<="
  | Ge -> printf ">="
  | Ne -> printf "!="

let show_constrain (c, s) = show_comp c; printf " %s" s

let rec show_formula atom f =
  match f with
  | And (f1, f2) ->
     printf "("; show_formula atom f1; printf " & "; show_formula atom f2;
     printf ")"
  | List [x] -> show_formula atom x
  | List l ->
     printf "(";
     List.iter (fun x -> show_formula atom x; printf " ") l;
     printf ")"
  | Or (f1, f2) ->
     printf "("; show_formula atom f1; printf " | "; show_formula atom f2;
     printf ")"
  | Not (f) -> printf "!"; show_formula atom f
  | Atom x -> atom x

let show_formula_nl label atom f =
  printf "%s = [" label;
  begin match f with
  | List l ->
     List.iter (fun x -> printf "\n  "; show_formula atom x) l;
     printf "\n"
  | _ -> show_formula atom f
  end;
  printf "]\n"

let show_package (s, o) =
  printf "%s" s;
  match o with
  | None | Some (List []) -> ()
  | Some f ->
     printf " {";
     show_formula show_constrain f;
     printf " }"

let show_filter (s, o) =
  printf "%s" s;
  match o with
  | None -> ()
  | Some c -> printf " "; show_constrain c

let show_item = function
  | Name s -> printf "name = %s\n" s
  | Version s -> printf "version = %s\n" s
  | Depends l -> show_formula_nl "depends" show_package l
  | Depopts l -> show_formula_nl "depopts" show_package l
  | Conflicts l ->
     show_list "conflicts" (fun p -> printf "  "; show_package p; printf "\n") l
  | Available f -> show_formula_nl "available" show_filter f
  | Ocaml_version l -> show_formula_nl "ocaml-version" show_constrain l
  | Skip -> ()
