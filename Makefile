all:main tests fuzzer

FILES = oracle.ml regex.ml bytecode.ml compiler.ml interpreter.ml linear.ml tojs.ml tore2.ml toexp.ml torust.ml jsmatcher.js complexity_exp.ml cdn.ml anchors.ml tests.ml charclasses.ml

%.native: %.ml $(FILES)
	ocamlbuild -package unix -package re2 -package core_bench -package core -package core_unix.command_unix $@


main: main.native
tests: tests.native
fuzzer: fuzzer.native

clean:
	-rm -R _build
	-rm *.native
	-rm *.data

clean_data:
	-rm *.csv
	-rm *.jpg
