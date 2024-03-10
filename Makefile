all:main.native

%.native: main.ml oracle.ml regex.ml bytecode.ml compiler.ml interpreter.ml linear.ml
	ocamlbuild -package unix -package re2 -package core_bench -package core -package core_unix.command_unix $@

clean:
	-rm -R _build
	-rm main.native
	-rm *.data

clean_data:
	-rm *.csv
	-rm *.jpg
