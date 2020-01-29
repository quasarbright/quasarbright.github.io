UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  NASM_FORMAT=elf64
  CLANG_FORMAT=-m64
else
ifeq ($(UNAME), Darwin)
  NASM_FORMAT=macho64
  CLANG_FORMAT=-m64
endif
endif


PKGS=oUnit,extlib,unix,str
BUILD=ocamlbuild -r -use-ocamlfind

%: %.ml
	$(BUILD) -package $(PKGS) $@.native
	mkdir -p runnables
	mv $@.native runnables/$@


# bf -> asm
output/%.s: input/%.bf runnables/main
	# ./runnables/main input/test.bf > output/test.s
	./runnables/main $< > $@

# asm -> binary
output/%.o: output/%.s
	# nasm -f ... -o output/test.o output/test.s
	nasm -f $(NASM_FORMAT) -o $@ $<

# c and asm binary -> run
output/%.run: output/%.o main.c
	# clang -g ... -o output/test.run main.c output/test.o
	clang -g $(CLANG_FORMAT) -o $@ main.c $<


clean:
	rm -rf output/*.o output/*.s output/*.dSYM output/*.run *.log runnables/
	ocamlbuild -clean
