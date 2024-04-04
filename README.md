# OCaml linear engine supporting streaming lookbehinds
Authors: [Aurèle Barrière](https://aurele-barriere.github.io/) and [Clément Pit-Claudel](https://pit-claudel.fr/clement/).

This is a modified version of the main engine in the `main` branch.
This one does not support all of the features of the main engine (for instance lookaheads), but it supports captureless lookbehinds in a streaming way, as seen on Section 4.4 of the paper.

## Dependencies
You need the following Opam packages.
Other version numbers may also work.
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

- `main.native` and `tests.native` execute some tests that should all succeed
- `fuzzer.native` is a fuzzer that compares the OCaml matcher to the Irregexp engine of V8 in Node

## Description of the Streaming Algorithm
Here are the main differences with the main development.

- the file `oracle.ml` now describes a one-dimensional table that corresponds to `LBtable` in the paper.
- you can see in `interpreter.ml`, line 379 how this table is initialized. It is now only as big as the number of lookarounds in the reges, and the size does not depend on the size of the string anymore.
- the instructions `CheckOracle`, `NegCheckOracle` and `WriteOracle` now manipulate this table. They correspond to the `CheckLB`, `NegCheckLB` and `WriteLB` instructions in the paper.
