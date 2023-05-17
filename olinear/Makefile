main.native: main.ml oracle.ml regex.ml bytecode.ml compiler.ml interpreter.ml linear.ml tojs.ml jsmatcher.js diff_fuzzer.ml complexity_exp.ml
	ocamlbuild -package unix main.native

clean:
	-rm -R _build
	-rm main.native
	-rm *.data

clean_data:
	-rm *.csv
	-rm *.jpg
