# OCaml Linear Engine for JS Regexes
Anonymous Authors

## Dependencies
You need the following Opam packages.
Oher version numbers may also work.
- Ocaml 5.0
- ocamlbuild 0.14.1
- Menhir 20220210
- ocaml_intrinsics v0.15.2
- core_bench v0.15.0
- core v0.15.1
- core_unix v0.15.2
- yojson 2.1.0

You also need to install Node.JS and have `node` in your path.

## Usage
Build all executables with `make`. 
Make sure to have configured your opam switch so that it has all dependencies listed above.
This creates several executables:

- `main.native` is the Ocaml matcher
- `fuzzer.native` is a fuzzer that compares the OCaml matcher to the Irregexp engine of V8 in Node
- `tests.native` contains a battery of tests that should all succeed
- `stats.native` computes regex feature usage statistics from corpora of regexes
- `benchmark.native` allows you to run benchmarks
- `matcher.native` and `linearbaseline.native` are only used for the benchmarks


`main.native`, `fuzzer.native` and `benchmark.native` have command line options that can be printed with the argument `--help`.

## Files
- the ECMA-style regex parser is defined in `parser_src/`
- regexes and regex annotation are found in `regex.ml`
- the extended bytecode NFA representation is defined in `bytecode.ml`
- compilation from a regex to bytecode is found in `compiler.ml`
- the NFA simulation algorithm, with all our extensions, is implemented in `interpreter.ml`
- the CDN plus formulas are defined in `cdn.ml`
- the oracles used y the lookaround algorithm is defined in `oracle.ml`
- the three capture registers implementations are defined in `regs.ml`
- character classes are implemented in `charclasses.ml`
- the differential fuzzer is implemented in `fuzzer.ml`
- the computation of statistics on regex features usage is defined in `stats.ml`

