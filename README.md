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
- `matcher.native` and `linearbaseline.native` are only used for the benchmarks and you should not run them directly


`main.native`, `fuzzer.native` and `benchmark.native` have command line options that can be printed with the argument `--help`.

## Files

### OCaml Engine Files
- the main enty point of the engine is in the `main.ml` file
- regexes and regex annotation are found in `regex.ml`
- the extended bytecode NFA representation is defined in `bytecode.ml`
- compilation from a regex to bytecode is found in `compiler.ml`
- the NFA simulation algorithm, with all our extensions, is implemented in `interpreter.ml`
- the CDN plus formulas are defined in `cdn.ml`
- the oracles used y the lookaround algorithm is defined in `oracle.ml`
- the three capture registers implementations are defined in `regs.ml`
- character classes are implemented in `charclasses.ml`
- the ECMA-style regex parser is defined in `parser_src/`

### Other Tools
- the differential fuzzer is implemented in `fuzzer.ml`
- the computation of statistics on regex features usage is defined in `stats.ml`
- the files `jsmatcher.js` and `jstimer.js` are executed by node when we compare against the node backtracking implementation in the benchmarks or in the fuzzer
- similarly `expmatcher.js` and `exptimer.js` are used to compare to the Experimental (or V8Linear) engine
- TODO: no! this was replaced now by the scripts in the scripts_bench dir! update

## Correspondance between the Paper and the Code

### Renamings
- The linear engine from V8 is called "V8Linear" in the paper. It is sometimes called "Experimental" or "Exp" in the code (as this is the name used by the V8 developers).
- The bytecode instructions `Consume` and `ConsumeAny` fro Figure 4 are replaced by a single `Consume` instruction in `bytecode.ml`, which takes as argument either a character or a list of character ranges.
- The `Jump` instruction is called `Jmp` in the code.
- `SetReg` is called `SetRegisterToCP`.
- `SetQuant` is called `SetQuantToClock`.
- `CheckNull` is called `CheckNullable`.
- `SetNullPlus` in the paper is not an independent instruction in the code. Instead `SetQuantToClock` encodes both `SetQuant` and `SetNullPlus`: it takes a boolean argument indicating if this quantifier is a nulled plus or not.
- The "Balanced Tree" register implementation in the paper is renamed to `Maps_Regs` in the code.

### Algorithm 1
This is implemented by the functions `advance_epsilon` and `find_match` in `interpreter.ml`.

### Section 4.1
- In `compiler.ml`, line 112, you can see the bytecode compilation of a quantifier and see that `BeginLoop` and `EndLoop` instructions are inserted.
- In `interpreter.ml`, the thread boolean `exit_allowed` encodes in which automata the thread is. See at lines 412 and 417 how the two new instructions are implemented.

### Section 4.2
- Threads are augmented with clocks in `quant_regs` (line 107 of `interpreter.ml`).
- The filtering algorithm is implemented at line 285 of `interpreter.ml`.

### Section 4.3
- The oracle table is defined in `oracle.ml`.
- The first phase is implemented as the `build_oracle` function, line 651 of `interpreter.ml`.
- The second phase is simply the `find_match` function defined previously.
- The third phase is implemented as the `build_capture` function, line 680 of `interpreter.ml`.

### Section 4.4
Switch to the `strlb` directory for this algorithm, and see the corresponding `README.md` file.

### Section 4.5
- The nullability analysis of Section 4.5.1 and Figure 12 starts at line 139 in `regex.ml`.
- The non-nullable plus case of Section 4.5.2 is implemented at line 78 in `compiler.ml`.
- For Section 4.5.3, see lines 90 and 101 of `compiler.ml`. 
- The nullability formulas of footnote 8 are defined in `cdn.ml` and called "CDN formulas".

### Section 4.6
- The three different register data-structures are defined in `regs.ml`: `Array_Regs`, `List_Regs` and `Map_Regs`.
- At line 28 of `interpreter.ml`, see that the interpreter is parameterized by a register implementation (a `REGS` module).

### Section 5.1
- All regex corpora are in the `corpus` directory.
- The `stats.ml` file uses the parser to analyze each regex and see in which categories it belongs to.

### Section 5.2
- (C1) the benchmark used for Figure 17 is named "Clocks", defined line 117 of `benchmark_vectors`.
- (C2) the benchmark used for Figure 18 is named "NNPlus", defined line 69.
- (C3) the benchmark used for Figure 19 is named "CDN", defined line 95.
- (C4) the benchmark used for Figure 20 is named "LBstr", defined line 179.
- (C5) the benchmark used for Figure 21 is named "LAreg", defined line 138.
- (C5) the benchmark used for Figure 22 is named "LAstr", defined line 157.
- TODO: maybe change in LAreg?
