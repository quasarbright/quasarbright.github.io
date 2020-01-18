
PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind

fsa: fsa.ml
	$(BUILD) -package $(PKGS) fsa.native
	mv fsa.native fsa

test: fsa.ml test.ml
	$(BUILD) -package $(PKGS) test.native
	mv test.native test

	
clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log
	ocamlbuild -clean
