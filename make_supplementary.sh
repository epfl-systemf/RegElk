set -e
rm supplementary.zip || true
rm -r supplementary || true
mkdir supplementary
mkdir supplementary/parser_src
mkdir supplementary/parser

cp anchors.ml bytecode.ml cdn.ml charclasses.ml compiler.ml flags.ml fuzzer.ml interpreter.ml jsmatcher.js main.ml oracle.ml regex.ml regs.ml tests.ml tojs.ml supplementary/
cp parser_src/regex_lexer.mll parser_src/regex_parser.mly supplementary/parser_src
cp supplementary_Makefile supplementary/Makefile
cp README supplementary/

zip -r supplementary.zip supplementary

