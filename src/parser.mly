(*
  parser.mly -- parsers for OPAM's [opam] and [url] files
  Copyright 2017 Inria
  author: Damien Doligez
*)

(* Declarations *)

%{ open Ast %}

%token EOF
%token <string> STRING
%token LBRACK RBRACK LBRACE RBRACE LPAREN RPAREN COLON
%token NOT EQ LT GT LE GE NE AND OR
%token <string> IDENT

%token NAME VERSION DEPENDS DEPOPTS CONFLICTS AVAILABLE OCAML_VERSION MIRRORS

%nonassoc NOT
%left AND
%left OR

%start <Ast.opam list> opam
%start <Ast.url> url

%%

(* Rules *)

opam :
| it = item* EOF { it }

item :
| NAME COLON s=STRING { Name s }
| VERSION COLON s=STRING { Version s }
| DEPENDS COLON LBRACK d=and_formula(package) RBRACK { Depends d }
| DEPENDS COLON d=formula(package) { Depends d }
| DEPOPTS COLON LBRACK d=and_formula(package) RBRACK { Depopts d }
| DEPOPTS COLON d=formula(package) { Depopts d }
| CONFLICTS COLON c=bracklist(package) { Conflicts c }
| AVAILABLE COLON LBRACK f=and_formula(filter) RBRACK { Available f }
| AVAILABLE COLON f=formula(filter) { Available f }
| OCAML_VERSION COLON LBRACK f=and_formula(constrain) RBRACK { Ocaml_version f }
| OCAML_VERSION COLON f=formula(constrain) { Ocaml_version f }
| IDENT COLON value { Skip }

value :
| STRING | ident { () }
| NOT | EQ | LT | GT | LE | GE | NE | AND | OR | COLON { () }
| LBRACK value* RBRACK { () }
| LPAREN value* RPAREN { () }
| value LBRACE value* RBRACE { () }

and_formula(x) :
| l=formula(x)* { match l with [x] -> x | _ -> List l }

formula(x) :
| f1=formula(x) AND f2=formula(x) { And (f1, f2) }
| f1=formula(x) OR f2=formula(x) { Or (f1, f2) }
| LPAREN f=and_formula(x) RPAREN { f }
| NOT f=formula(x) { Not f }
| x=x { Atom x }

package :
| s=STRING f=option( LBRACE flag* f=and_formula(constrain) RBRACE {f} )
  { (s, match f with Some (Ast.List []) -> None | _ -> f) }

filter :
| a=argument { (a, None) }
| a1=argument c=comp a2=argument { (a1, Some (c, a2)) }

argument :
| s=STRING { s }
| s=ident { s }

comp :
| EQ { Eq }
| LT { Lt }
| GT { Gt }
| LE { Le }
| GE { Ge }
| NE { Ne }

constrain :
| c=comp s=STRING { (c, s) }

brack(x) :
| LBRACK x=x RBRACK { x }
| x=x { x }

bracklist(x) :
| LBRACK l=x* RBRACK { l }
| x=x { [x] }

ident :
| s=IDENT { s }
| NAME { "name" }
| VERSION { "version" }
| DEPENDS { "depends" }
| DEPOPTS { "depopts" }
| CONFLICTS { "conflicts" }
| AVAILABLE { "available" }
| OCAML_VERSION { "ocaml-version" }
| MIRRORS { "mirrors" }

flag :
| IDENT { () }
| AND { () }

url :
| MIRRORS COLON LBRACK l=STRING+ RBRACK { Mirrors l }
| k=IDENT COLON s=STRING { Key (k, s) }
