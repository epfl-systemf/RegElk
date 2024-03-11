all:main.native fuzzer.native tests.native

%.native: main.ml oracle.ml regex.ml bytecode.ml compiler.ml interpreter.ml linear.ml fuzzer.ml tests.ml
	ocamlbuild -package unix -package core_bench -package core -package core_unix.command_unix $@

clean:
	rm -f -R _build
	rm -f *.native
	rm -f *.data

clean_data:
	rm -f *.csv
	rm -f *.jpg
