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
open Diff_fuzzer
open Complexity_exp
open Tests

  
let main =
  (* let bug = (Raw_count({min=1;max=Some 3;greedy=false},Raw_dot),"ab") in
   * ignore (get_linear_result ~verbose:true ~debug:true (fst bug) (snd bug)) *)

    
  (* tests() *)
  fuzzer()
    
  (* run_benchmark(many_forks); *)
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
