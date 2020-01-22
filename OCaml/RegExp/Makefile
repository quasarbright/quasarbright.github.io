
PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind

fsa: fsa.ml
	$(BUILD) -package $(PKGS) fsa.native
	mv fsa.native fsa

common: common.ml
	$(BUILD) -package $(PKGS) common.native
	mv common.native common


regexp: common.ml fsa.ml regexp.ml
	$(BUILD) -package $(PKGS) regexp.native
	mv regexp.native regexp

test: common.ml fsa.ml regexp.ml test.ml
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

	
clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	ocamlbuild -clean
