
PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind

fsa: fsa.ml
	$(BUILD) -package $(PKGS) fsa.native
	mv fsa.native fsa

iterator: iterator.ml
	$(BUILD) -package $(PKGS) iterator.native
	mv iterator.native iterator

common: common.ml
	$(BUILD) -package $(PKGS) common.native
	mv common.native common

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
