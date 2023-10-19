(** * Benchmarks  *)
(* This file is here to replicate the pictures in the paper *)
(* see also the file complexity_exp.ml for other experiments *)

open Regex
open Tojs
open Toexp
open Interpreter
open Sys
open Unix
open Gc
open Flags
open Arg


(* path to V8 executable *)
(* this V8 executable needs to be patched in two ways: *)
(* - first, we augment the kMaxReplicationFactor, so that we can try more regexes to demonstrate regex-size-exponential complexity *)
(* second, we define performance.rdtsc() to return rdtsc counter *)
let v8_path = ref "~/v8/v8/out/x64.release/d8"
let v8_args = " --expose-gc --enable-experimental-regexp-engine "

   
(* where the results are stored *)
let bench_dir = "results_bench/"
(* where we store the argument to the d8 matcher *)
let param_path = "scripts_bench/v8params.js"
(* the V8 linear program that measures time *)
let v8lineartimer = "scripts_bench/v8lineartimer.js"

(* the number of times we warmup before each test *)
let warmups = ref 10
(* the number of times we perform each test *)
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


(* calling the matcher.native executable *)
(* returning the rdtsc measuring *)
let get_time_ocaml (r:raw_regex) (str:string): string =
  let regex_string = " \""^print_js r^"\" " in
  let input_string = " \""^str^"\" " in
  let sys = "./matcher.native " ^ regex_string ^ input_string ^ string_of_int !warmups in
  string_of_command(sys)

(* modifies the parameter files that is read by the V8 D8 interpreter *)
let write_v8_params (r:raw_regex) (str:string): unit =
  let oc = open_out param_path in
  Printf.fprintf oc "const regex= \"%s\"\nconst string= \"%s\"\nconst warmups= %d\n" (print_js r) str !warmups;
  close_out oc


let get_time_v8linear (r:raw_regex) (str:string): string =
  write_v8_params r str;
  let sys = !v8_path ^ v8_args ^ v8lineartimer in
  string_of_command(sys)

(* measures rdtsc time for each engine *)
let get_time (e:engine) (r:raw_regex) (str:string) : string =
  match e with
  | OCaml -> get_time_ocaml r str
  | V8Linear -> get_time_v8linear r str
  | Irregexp -> failwith "TODO: bench Irregexp"


(* an engine configuration to test *)
type engine_conf =
  { eng: engine;                (* which engine *)
    min_size: int;              (* minimum size of the benchmark *)
    max_size: int               (* maximum size of the benchmark *)
  }

              
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
    for j = 0 to (!repetitions-1) do
      let time = get_time ec.eng reg str in
      Printf.fprintf oc "%d,%s%!" i time; (* printing to the csv file *)
    done;
  done;
  close_out oc;
  Printf.printf "\n%!";
  Unix.sleep 1

let run_regex_benchmark (rb:regex_benchmark) : unit =
  Printf.printf ("Generating .csv files for benchmark %s:\n") (rb.name);
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
  [ {eng=OCaml; min_size=0; max_size=500 };
    {eng=V8Linear; min_size=0; max_size=20 } ]

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
  (* run_regex_benchmark clocks; *)
  ()
    
