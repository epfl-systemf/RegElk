main.native: main.ml oracle.ml regex.ml bytecode.ml compiler.ml interpreter.ml linear.ml tojs.ml tore2.ml toexp.ml jsmatcher.js diff_fuzzer.ml complexity_exp.ml
	ocamlbuild -package unix -package re2 main.native

clean:
	-rm -R _build
	-rm main.native
	-rm *.data

clean_data:
	-rm *.csv
	-rm *.jpg
