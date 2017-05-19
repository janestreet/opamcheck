# Makefile -- build instructions for opamcheck
# Copyright 2017 Inria
# author: Damien Doligez

COMPFLAGS = -annot
LINKFLAGS =

OPTCOMPFLAGS = ${COMPFLAGS}
OPTLINKFLAGS = ${LINKFLAGS}

BYTCOMPFLAGS = ${COMPFLAGS} -g
BYTLINKFLAGS = ${LINKFLAGS} -g

MODULES = status env vdd ast parser lexer \
          version toposort package solver sandbox

MLI = ${MODULES:=.mli}
ML = ${MODULES:=.ml} main.ml

CMI = ${ML:.ml=.cmi}
CMO = ${ML:.ml=.cmo}
CMX = ${ML:.ml=.cmx}

.PHONY: all
all: .depend
	${MAKE} opamcheck.byt opamcheck.opt

opamcheck.byt: ${CMO}
	ocamlc ${BYTLINKFLAGS} $^ -o $@

opamcheck.opt: ${CMX}
	ocamlopt ${OPTLINKFLAGS} $^ -o $@

parser.mli : parser.ml

parser.ml : parser.mly
	menhir parser.mly

lexer.ml : lexer.mll
	ocamllex lexer.mll

%.cmx: %.ml
	ocamlopt ${OPTCOMPFLAGS} -c $< -o $@

%.cmo : %.ml
	ocamlc ${BYTCOMPFLAGS} -c $< -o $@

%.cmi : %.mli
	ocamlc ${BYTCOMPFLAGS} -c $< -o $@

clean:
	rm -f *.cm[iox] *.o *.annot
	rm -f opamcheck lexer.ml parser.mli parser.ml

.PHONY: depend
depend: .depend

.depend: ${MLI} ${ML}
	ocamldep ${MLI} ${ML} >.depend

include .depend
