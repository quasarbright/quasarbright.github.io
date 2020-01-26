ocamllex lexer.mll
mv lexer.ml _build/
ocamlyacc parser.mly
mv parser.ml _build/
mv parser.mli _build/