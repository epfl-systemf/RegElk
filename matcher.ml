open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Interpreter
open Tojs
open Tore2
open Toexp
open Torust
open Todotnet
open Charclasses
open Complexity_exp
open Flags

(** * Measuring With RDTSC *)
(* all our algorithms are measured with the rdtsc instruction *)
(* to get a estimate as precise as possible *)
(* when we measure V8Linear, we also patch it to return rdtsc *)
(* we use the rdtsc interface provided by the Ocaml_Intrinsics package *)
let now () = Ocaml_intrinsics.Perfmon.rdtsc ()
let elapsed from = Int64.sub (now ()) from

(** * Measuring The OCaml engine execution  *)
(* This executable is to be called directly by the benchmarks *)
   
(* Executing the OCaml linear engine on a regex and a string *)
(* Expects exactly 3 arguments:
- the regex
- the input string
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
  (* disabling debug/verbose output *)
  debug := false;
  verbose := false;
  
  let regex = Sys.argv.(1) in
  let string = Sys.argv.(2) in
  let warmups = int_of_string(Sys.argv.(3)) in

  (* building the regex *)
  let parsed_regex = parse_raw regex in
  (* annotating the regex *)
  let annotated_regex = annotate parsed_regex in
  (* compiling the regex *)
  let compiled_regex = full_compilation annotated_regex in

  (* Warmup *)
  (* this shouldn't change anything for this engine *)
  (* but we do it anyway to mirror our evaluation of the V8Linear engine *)
  for i=0 to (warmups-1) do
    ignore(matcher compiled_regex string)
  done;

  (* triggering garbage collector *)
  Gc.full_major();

  (* measuring one match *)
  let tstart = now() in
  let o = build_oracle compiled_regex string in
  let _ = build_capture compiled_regex string o in
  let time = elapsed tstart in
  
  Printf.printf ("%Li\n") time
 
