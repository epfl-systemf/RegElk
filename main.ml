open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Interpreter
open Linear
open Tojs
open Tore2
open Toexp
open Torust
open Charclasses
open Diff_fuzzer
open Complexity_exp
open Tests

  
let main =
  
  (* let bug = (Raw_class([CRange(char_of_int(39),char_of_int(97))]),"-babbababb-b--aaaaa-aaa-bab-b--") in
   * ignore (get_linear_result ~verbose:true ~debug:true (fst bug) (snd bug));
   * ignore (compare_engines (fst bug) (snd bug)) *)
    

  tests()
  (* fuzzer() *)
    
  (* run_benchmark(many_forks) *)
  (* many_forks_rust_benchmark() *)

  (* TODO: make more JSCore benchmarks *)
  (* let open Core in
   *   let open Core_bench in
   *   let (array_args,matcher_fn,name) = prepare_core_benchmark quadratic_plus in
   *   Command_unix.run (Bench.make_command [
   *                    Bench.Test.create_indexed
   *                      ~name
   *                      ~args:(List.init (Array.length array_args) (fun i -> i))
   *                      matcher_fn ]) *)
