(* lexer.mll -- lexical analyzer for OPAM's opam and url files
   Copyright 2017 Inria
   author: Damien Doligez
*)

{
open Parsing_aux
open Parser
exception Error of int

let count_lf s =
  let rec loop i n =
    if i >= String.length s then n
    else if s.[i] = '\n' then loop (i+1) (n+1)
    else loop (i+1) n
  in
  loop 0 0
}

rule token = parse
| [ ' ' '\t' '\r' ] { token lexbuf }
| "\n" { incr line; token lexbuf }
| "#" [^ '\n']* "\n" { incr line; token lexbuf }
| "(*" { comment 0 lexbuf }
| "\"" (([^ '"' '\\'] | "\\" _)* as s) "\"" {
   line := !line + count_lf s;
   STRING s
}
| "(" { LPAREN }
| ")" { RPAREN }
| "[" { LBRACK }
| "]" { RBRACK }
| "{" { LBRACE }
| "}" { RBRACE }
| ":" { COLON }
| "|" { OR }
| "&" { AND }
| "!" { NOT }
| "=" { EQ }
| "!=" { NE }
| "<" { LT }
| ">" { GT }
| "<=" { LE }
| ">=" { GE }
| "name" { NAME }
| "version" { VERSION }
| "depends" { DEPENDS }
| "depopts" { DEPOPTS }
| "conflicts" { CONFLICTS }
| "available" { AVAILABLE }
| "ocaml-version" { OCAML_VERSION }
| "mirrors" { MIRRORS }
| [ 'a' - 'z' 'A' - 'Z']
  (['\033' - '\126'] # [ '(' ')' '[' ']' '{' '}' ':' '=' '<' '>'])*
  as id { IDENT id }
| eof { EOF }

and comment level = parse
| "(*" { comment (level+1) lexbuf }
| "*)" { if level = 0
         then token lexbuf
         else comment (level-1) lexbuf
       }
| '\n' { incr line; comment level lexbuf }
| eof { raise (Error !line) }
| [^ '(' '*' '\n']+ { comment level lexbuf }
| _ { comment level lexbuf }

{

open Printf

let print t =
  match t with
  | STRING s -> printf "STRING %S\n" s
  | IDENT s -> printf "IDENT %S\n" s
  | LPAREN -> printf "LPAREN\n"
  | RPAREN -> printf "RPAREN\n"
  | LBRACK -> printf "LBRACK\n"
  | RBRACK -> printf "RBRACK\n"
  | LBRACE -> printf "LBRACE\n"
  | RBRACE -> printf "RBRACE\n"
  | COLON -> printf "COLON\n"
  | OR -> printf "OR\n"
  | AND -> printf "AND\n"
  | NOT -> printf "NOT\n"
  | EQ -> printf "EQ\n"
  | NE -> printf "NE\n"
  | LT -> printf "LT\n"
  | GT -> printf "GT\n"
  | LE -> printf "LE\n"
  | GE -> printf "GE\n"
  | NAME -> printf "NAME\n"
  | VERSION -> printf "VERSION\n"
  | DEPENDS -> printf "DEPENDS\n"
  | DEPOPTS -> printf "DEPOPTS\n"
  | CONFLICTS -> printf "CONFLICTS\n"
  | AVAILABLE -> printf "AVAILABLE\n"
  | OCAML_VERSION -> printf "OCAML_VERSION\n"
  | MIRRORS -> printf "MIRRORS\n"
  | EOF -> printf "EOF\n"

}
