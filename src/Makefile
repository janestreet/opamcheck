# Makefile -- build instructions for opamcheck
# Copyright 2017 Inria
# author: Damien Doligez

PACKAGES = -package minisat

COMPFLAGS = ${PACKAGES} -annot
LINKFLAGS = ${PACKAGES}

OPTCOMPFLAGS = ${COMPFLAGS}
OPTLINKFLAGS = ${LINKFLAGS}

BYTCOMPFLAGS = ${COMPFLAGS} -g
BYTLINKFLAGS = ${LINKFLAGS} -g

MODULES = util status env ast parsing_aux parser lexer \
          version toposort package solver sandbox

LIBS = unix minisat

MLI = ${MODULES:=.mli}
ML = ${MODULES:=.ml} main.ml

CMI = ${ML:.ml=.cmi}
CMO = ${ML:.ml=.cmo}
CMX = ${ML:.ml=.cmx}
CMA = ${LIBS:=.cma}
CMXA = ${LIBS:=.cmxa}

.PHONY: all
all: .depend
	${MAKE} opamcheck.byt opamcheck.opt

opamcheck.byt: ${CMO}
	ocamlfind ocamlc ${BYTLINKFLAGS} ${CMA} $^ -o $@

opamcheck.opt: ${CMX}
	ocamlfind ocamlopt ${OPTLINKFLAGS} ${CMXA} $^ -o $@

parser.mli : parser.ml

parser.ml : parser.mly
	menhir parser.mly

lexer.ml : lexer.mll
	ocamllex lexer.mll

%.cmx: %.ml
	ocamlfind ocamlopt ${OPTCOMPFLAGS} -c $< -o $@

%.cmo : %.ml
	ocamlfind ocamlc ${BYTCOMPFLAGS} -c $< -o $@

%.cmi : %.mli
	ocamlfind ocamlc ${BYTCOMPFLAGS} -c $< -o $@

clean:
	rm -f *.cm[iox] *.o *.annot
	rm -f opamcheck lexer.ml parser.mli parser.ml

.PHONY: depend
depend: .depend

.depend: ${MLI} ${ML}
	ocamldep ${MLI} ${ML} >.depend

include .depend
