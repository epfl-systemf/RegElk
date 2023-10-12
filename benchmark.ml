(** * Benchmarks  *)
(* This file is here to replicate the pictures in the paper *)
(* see also the file complexity_exp.ml for other experiments *)

open Regex
open Tojs
open Toexp
open Linear
open Sys
open Unix
open Gc
open Flags
open Arg


let bench_dir = "results_bench/"

(* the number of times we reapeat each test *)
let repetitions = ref 10

type engine =
  | OCaml
  | V8Linear
  | Irregexp

let engine_name (e:engine) : string =
  match e with
  | OCaml -> "OCaml"
  | V8Linear -> "V8linear"
  | Irregexp -> "Irregexp"

(* an engine configuration to test *)
type engine_conf =
  { eng: engine;                (* which engine *)
    min_size: int;              (* minimum size of the benchmark *)
    max_size: int               (* maximum size of the benchmark *)
  }

(* also measures compilation time... *)
(* TODO: change this? *)
let get_time_ocaml (r:raw_regex) (str:string) : float =
  Gc.full_major();               (* triggering the GC *)
  let tstart = Unix.gettimeofday() in
  ignore(get_linear_result r str);
  let tend = Unix.gettimeofday() in
  tend -. tstart

let get_time (e:engine) (r:raw_regex) (str:string) : string =
  match e with
  | OCaml -> string_of_float (get_time_ocaml r str)
  | V8Linear -> get_time_experimental r str
  | Irregexp -> get_time_js r str

(* A benchmark where we vary the length of the regex *)
type regex_benchmark =
  { name: string;
    confs: engine_conf list;       (* list of all engine configurations to test *)
    param_regex: int -> raw_regex; (* the family of regexes *)
    input_str: string;             (* the string on which to match *)
  }
    
(* runs a benchmark on a single engine and prints the result to a csv file *)
let run_regex_config (ec:engine_conf) (param_regex:int->raw_regex) (str:string) (name:string) : unit =
  Printf.printf "Testing engine %s:\n%!" (engine_name ec.eng);
  let oc = open_out (bench_dir^name^"_"^(engine_name ec.eng)^".csv") in
  for i = ec.min_size to ec.max_size do
    Printf.printf " %s\r%!" (string_of_int i); (* live update *)
    let reg = param_regex i in
    for j = 0 to !repetitions do
      let time = get_time ec.eng reg str in
      Printf.fprintf oc "%d,%s\n%!" i time; (* printing to the csv file *)
    done;
  done;
  close_out oc;
  Printf.printf "\n%!";
  Unix.sleep 1

let run_regex_benchmark (rb:regex_benchmark) : unit =
  Printf.printf ("generating .csv files for benchmark %s:\n") (rb.name);
  List.iter (fun rc -> run_regex_config rc rb.param_regex rb.input_str rb.name) rb.confs


(** * Defining benchmarks *)


(** * Nested Non-Nullable Plus  *)
(* r0 = a *)
(* rn = rn-1+ *)
let rec nested_nn_plus_reg = fun reg_size ->
  match reg_size with
  | 0 -> raw_char('a')
  | _ -> raw_plus(nested_nn_plus_reg (reg_size - 1))

let nested_nn_plus_string = String.make 100 'a'

let nn_plus_confs =
  [ {eng=OCaml; min_size=0; max_size=100 };
    {eng=V8Linear; min_size=0; max_size=4 } ]

let nested_nn_plus : regex_benchmark =
  { name = "NNPlus";
    confs = nn_plus_confs;
    param_regex = nested_nn_plus_reg;
    input_str = nested_nn_plus_string;
  }
  

(** * Clocks have a better complexity than dynamic clearing of registers  *)
(* r0 = . *)
(* rn = (rn-1)* *)
let rec clocks_reg = fun reg_size ->
  match reg_size with
  | 0 -> raw_dot
  | _ -> raw_star(Raw_capture(clocks_reg (reg_size - 1)))

let clocks_string = String.make 100 'a'

let clocks_conf =
  [ {eng=OCaml; min_size=0; max_size=300 };
    {eng=V8Linear; min_size=0; max_size=300 } ]

let clocks : regex_benchmark =
  { name = "Clocks";
    confs = clocks_conf;
    param_regex = clocks_reg;
    input_str = clocks_string;
  }
              
  

let main =
  run_regex_benchmark nested_nn_plus;
  run_regex_benchmark clocks;
  ()
    
