all:main tests fuzzer

FILES = oracle.ml regex.ml bytecode.ml compiler.ml interpreter.ml linear.ml tojs.ml tore2.ml toexp.ml torust.ml jsmatcher.js complexity_exp.ml cdn.ml anchors.ml tests.ml charclasses.ml flags.ml parser.ml

PARSER_SRC = parser_src/regex_lexer.mll parser_src/regex_parser.mly
PARSER_FILES = parser/regex_lexer.ml parser/regex_parser.ml

parser/regex_lexer.ml: parser_src/regex_lexer.mll
	ocamllex -o parser/regex_lexer.ml parser_src/regex_lexer.mll

parser/regex_parser.ml: parser_src/regex_parser.mly
	menhir parser_src/regex_parser.mly
	mv parser_src/regex_parser.ml parser_src/regex_parser.mli parser

parser_clean:
	-rm parser/*

%.native: %.ml $(FILES) $(PARSER_FILES) $(PARSER_SRC)
	ocamlbuild -I parser -package unix -package re2 -package core_bench -package core -package core_unix.command_unix $@


main: main.native
tests: tests.native
fuzzer: fuzzer.native

clean: parser_clean
	-rm -R _build
	-rm *.native
	-rm *.data

clean_data:
	-rm *.csv
	-rm *.jpg
