
PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind

%: %.ml
	$(BUILD) -package $(PKGS) $@.native
	mkdir -p runnables
	mv $@.native runnables/$@

clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log runnables/
	ocamlbuild -clean
