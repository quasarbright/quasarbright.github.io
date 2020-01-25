
PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind

all: clean fsa iterator common lexer parser binTree satisfy regexp satisfyMain test

fsa: fsa.ml
	$(BUILD) -package $(PKGS) fsa.native
	mv fsa.native fsa

iterator: iterator.ml
	$(BUILD) -package $(PKGS) iterator.native
	mv iterator.native iterator

common: common.ml
	$(BUILD) -package $(PKGS) common.native
	mv common.native common

lexer: lexer.mll
	ocamllex lexer.mll
	mv lexer.ml _build/
	$(BUILD) -package $(PKGS) lexer.native
	mv lexer.native lexer

parser: parser.mly
	ocamlyacc parser.mly
	mv parser.ml _build/
	mv parser.mli _build/
	$(BUILD) -package $(PKGS) parser.native
	mv parser.native parser

binTree: binTree.ml
	$(BUILD) -package $(PKGS) binTree.native
	mv binTree.native binTree

satisfy: common.ml fsa.ml
	$(BUILD) -package $(PKGS) satisfy.native
	mv satisfy.native satisfy

regexp: common.ml fsa.ml regexp.ml
	$(BUILD) -package $(PKGS) regexp.native
	mv regexp.native regexp

satisfyMain: common.ml fsa.ml regexp.ml satisfyMain.ml
	$(BUILD) -package $(PKGS) satisfyMain.native
	mv satisfyMain.native satisfyMain

test: common.ml fsa.ml regexp.ml test.ml
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	ocamlbuild -clean
