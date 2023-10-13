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
open Todotnet
open Charclasses
open Complexity_exp
open Flags

(* This executable is to be called directly by the benchmarks *)
   
(* Executing the OCaml linear engine on a regex and a string *)
(* Expects exactly 4 arguments:
- the regex
- the input string
- the number of repetitions to measure
- the number of warmup repetitions *)
(* Prints the total time in seconds *)

let input_str = ref ""
let input_regex = ref ""
let str_set = ref false
let rgx_set = ref false
let compare_js = ref false 
   
(* fails if the regex is not correct *)
let parse_raw (str:string) : raw_regex =
  let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
  assert (regex_wf r);
  r

  
let main =
  let regex = Sys.argv.(1) in
  let string = Sys.argv.(2) in
  let repetitions = int_of_string(Sys.argv.(3)) in
  let warmups = int_of_string(Sys.argv.(4)) in

  (* building the regex *)
  let parsed_regex = parse_raw regex in

  (* Warmup *)
  for i=0 to (warmups-1) do
    ignore(get_linear_result parsed_regex string)
  done;

  (* triggering garbage collector *)
  Gc.full_major();

  (* measuring the repetitions *)
  let tstart = Unix.gettimeofday() in
  for i=0 to (repetitions-1) do
    ignore(get_linear_result parsed_regex string)
  done;
  let tend = Unix.gettimeofday() in
  
  Printf.printf ("%f\n") (tend -. tstart)
 
