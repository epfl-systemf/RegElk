open Regex
open Regs

type engine =
  | OCaml
  | OCamlBench
  | OldV8Linear
  | NewV8Linear
  | Irregexp
  | LinearBaseline

let engine_name (e:engine) : string =
  match e with
  | OCaml -> "OCaml"
  | OCamlBench -> "OCamlBench"
  | OldV8Linear -> "oldV8L"
  | NewV8Linear -> "newV8L"
  | Irregexp -> "Irregexp"
  | LinearBaseline -> "LinearBaseline"

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

type string_benchmark =
  { name: string;
    confs: engine_conf list;
    param_str: int -> string;
    rgx: raw_regex;
  }

type benchmark =
  | RB of regex_benchmark
  | SB of string_benchmark

let bench_name (b:benchmark) : string =
  match b with
  | RB r -> r.name
  | SB s -> s.name

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
  [ {eng=NewV8Linear; min_size=0; max_size=1000 };
    {eng=OldV8Linear; min_size=0; max_size=20 } ]

let nested_nn_plus : regex_benchmark =
  { name = "NNPlus";
    confs = nn_plus_confs;
    param_regex = nested_nn_plus_reg;
    input_str = nested_nn_plus_string;
  }

(** * Nested CDN Plus  *)
(* I picked an example with anchors rather than lookarounds *)
(* so that we can compare the results to V8Linear *)
(* I picked an example where all CDNs end up nulled and *)
(* All CDNs need to be launched again to reconstruct a group *)

(* r0 = a|(^) *)
(* rn = rn-1+ *)
let rec nested_cdn_reg = fun reg_size ->
  match reg_size with
  | 0 -> Raw_alt(raw_char('a'),Raw_capture(Raw_anchor(BeginInput)))
  | _ -> raw_plus(nested_cdn_reg (reg_size - 1))

let nested_cdn_string = "b"

let nested_cdn_confs =
  [ {eng=OCaml; min_size=0; max_size=1000 };
    {eng=OldV8Linear; min_size=0; max_size=20 } ]

let nested_cdn : regex_benchmark =
  { name = "CDN";
    confs = nested_cdn_confs;
    param_regex = nested_cdn_reg;
    input_str = nested_cdn_string;
  }


(** * Clocks have a better complexity than dynamic clearing of registers  *)
(* r0 = . *)
(* rn = (rn-1)* *)
let rec clocks_reg = fun reg_size ->
  match reg_size with
  | 0 -> raw_char('a')
  | _ -> raw_star(Raw_capture(clocks_reg (reg_size - 1)))

let clocks_string = String.make 100 'a'

let clocks_conf =
  [ {eng=OCaml; min_size=0; max_size=500 };
    {eng=OldV8Linear; min_size=0; max_size=500 } ]

let clocks : regex_benchmark =
  { name = "Clocks";
    confs = clocks_conf;
    param_regex = clocks_reg;
    input_str = clocks_string;
  }

(** * Lookarounds Regex-Size *)
(* r0 = ( a* )b *)
(* rn = a (?= rn-1 ) *)
let rec nested_look_reg = fun reg_size ->
  match reg_size with
  | 0 -> Raw_con(Raw_capture(raw_star(raw_char('a'))),raw_char('b'))
  | _ -> Raw_con(raw_char('a'),Raw_lookaround(Lookahead,nested_look_reg (reg_size -1)))

let nested_look_reg_str = String.make 1000 'a' ^ "b"

let nested_look_conf =
  [ {eng=OCaml; min_size=0; max_size=1000 };
    {eng=Irregexp; min_size=0; max_size=1000 } ]

let nested_lookarounds : regex_benchmark =
  { name = "LAreg";
    confs = nested_look_conf;
    param_regex = nested_look_reg;
    input_str = nested_look_reg_str;
  }

  
(** * Lookarounds String-Size  *)
(* c (?: a (?= a* (?<=c (a* ) ) b ) )* *)
let nested_la_reg = Raw_con(raw_char('c'),raw_star(Raw_con(raw_char('a'),Raw_lookaround(Lookahead,Raw_con(raw_star(raw_char('a')),Raw_con(Raw_lookaround(Lookbehind,Raw_con(raw_char('c'),Raw_capture(raw_star(raw_char('a'))))),raw_char('b')))))))

let nested_la_param_str = fun str_size ->
  "c" ^ String.make str_size 'a' ^ "b"

let nested_look_str_conf =
  [ {eng=OCaml; min_size=0; max_size=5000 }; 
    {eng=Irregexp; min_size=0; max_size=5000 } ]

let nested_lookarounds_string : string_benchmark =
  { name = "LAstr";
    confs = nested_look_str_conf;
    param_str = nested_la_param_str;
    rgx = nested_la_reg;
  }


(** * Lookbehinds String-Size  *)
(* b(?: a (?<= a* b) )* *)
let nested_lb_reg = Raw_con(raw_char('b'),raw_star(Raw_con(raw_char('a'),Raw_lookaround(Lookbehind,Raw_con(raw_char('b'),raw_star(raw_char('a')))))))

let nested_lb_param_str = fun str_size ->
  "b" ^ String.make str_size 'a'

let nested_lookb_str_conf =
  [ (* {eng=LinearBaseline; min_size=0; max_size=5000 }; *)
    {eng=OCaml; min_size=0; max_size=5000 };
    (* {eng=OCamlBench; min_size=0; max_size=3000 }; *)
    {eng=NewV8Linear; min_size=0; max_size=5000 };
    {eng=Irregexp; min_size=0; max_size=5000 } ]

let nested_lookbehinds_string : string_benchmark =
  { name = "LBstr";
    confs = nested_lookb_str_conf;
    param_str = nested_lb_param_str;
    rgx = nested_lb_reg;
  }

(** * Lookbehinds Regex-Size *)
(* r0 = ba* *)
(* rn = a* (?<= rn-1 ) *)
let rec nested_lb_reg = fun reg_size ->
  match reg_size with
  | 0 -> Raw_con(raw_char('b'),raw_star(raw_char('a')))
  | _ -> Raw_con(raw_star(raw_char('a')),Raw_lookaround(Lookbehind,nested_lb_reg (reg_size -1)))

let nested_lb_reg_str = "b" ^ String.make 100 'a'

let nested_lb_conf =
  [ {eng=NewV8Linear; min_size=0; max_size=1000 }; ]
    (* {eng=Irregexp; min_size=0; max_size=300 } ] *)

let nested_lb : regex_benchmark =
  { name = "LBreg";
    confs = nested_lb_conf;
    param_regex = nested_lb_reg;
    input_str = nested_lb_reg_str;
  }


(** * Register Data-Structure Benchmark  *)
(* The goal is to launch this benchmark several times, switching the register implementation in the interpreter *)
(* Later I'll make the interpreter a functor to switch dynamically, here you need to recompile each time. *)
(* we'll compare the results of each register data_structure (ds) *)

let rec ds_reg = fun reg_size ->
  match reg_size with
  | 0 -> raw_qmark(Raw_capture(raw_char('a')))
  | _ -> Raw_con(raw_qmark(Raw_capture(raw_char('a'))),ds_reg (reg_size - 1))

let ds_param_reg = fun reg_size ->
  raw_star(ds_reg reg_size)

let ds_str = String.make 1000 'a'

(* for array_regs, you can stop at ~200 or it really becomes long *)
let ds_conf =
  [{eng=OCaml; min_size=0; max_size=500}]

let dsarray : regex_benchmark =
  { name = Array_Regs.name;
    confs = ds_conf;
    param_regex = ds_param_reg;
    input_str = ds_str }

let dslist : regex_benchmark =
  { name = List_Regs.name;
    confs = ds_conf;
    param_regex = ds_param_reg;
    input_str = ds_str }

let dstree : regex_benchmark =
  { name = Map_Regs.name;
    confs = ds_conf;
    param_regex = ds_param_reg;
    input_str = ds_str }


let all_bench : benchmark list =
  [RB nested_nn_plus; RB nested_cdn; RB clocks;
   RB nested_lookarounds; SB nested_lookarounds_string;
   RB nested_lb; SB nested_lookbehinds_string;
   RB dsarray; RB dslist; RB dstree]

let bench_names = List.map (fun b -> bench_name b) all_bench
let bench_names_string =
  List.fold_left (fun a b -> a ^ " " ^ b) "" bench_names

let benchmark_vector_of_string name =
  try
    List.find (fun b -> bench_name b = name) all_bench
  with Not_found -> (
    Printf.printf "Couldn't find benchmark %s\nAvailable benchmarks: %s\n" name bench_names_string;
    exit 1
  )
