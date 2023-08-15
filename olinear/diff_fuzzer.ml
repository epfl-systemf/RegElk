(** * Differential Fuzzer  *)
(* randomly generating regexes and strings and comparing the output of our algorithm VS the JS matcher *)

open Linear
open Tojs
open Regex
open Random

let _ = Random.self_init()


      
(* we restrict ourselves to a small alphabet *)
let alphabet = ['a'; 'b'; 'c']

let max_depth = 50

let max_string = 100

let max_tests = 1000

(** * Creating Random Regexes  *)
   
let random_char () : char =
  let idx = Random.int (List.length alphabet) in
  List.nth alphabet idx

let random_quant () : quantifier =
  match (Random.int 4) with
  | 0 -> Star
  | 1 -> LazyStar
  | 2 -> Plus
  | 3 -> LazyPlus
  | _ -> failwith "random range error"

let random_look () : lookaround =
  match (Random.int 2) with
  | 0 -> Lookbehind
  | 1 -> NegLookbehind
  | _ -> failwith "random range error"

(* with a maximal number of recursion [depth] *)
(* the [look] boolean specifies if lookarounds are allowed *)
(* the [cap] boolean specifies if capture groups are allowed *)
let rec random_regex (depth:int) (look:bool) (cap:bool): raw_regex =
  let max = if look then 10 else 8 in
  let rand = if (depth=0) then Random.int 3 else Random.int max in
  match rand with
  | 0 -> Raw_empty
  | 1 -> let x = random_char() in Raw_char x
  | 2 -> Raw_dot
  | 3 ->
     let r1 = random_regex (depth-1) look cap in
     let r2 = random_regex (depth-1) look cap in
     Raw_alt (r1, r2)
  | 4 ->
     let r1 = random_regex (depth-1) look cap in
     let r2 = random_regex (depth-1) look cap in
     Raw_con (r1, r2)
  | 5 ->
     let r1 = random_regex (depth-1) look cap in
     let q = random_quant() in
     Raw_quant(q, r1)
  | 6 | 7 ->
     if cap then begin
         let r1 = random_regex (depth-1) look cap in
         Raw_capture(r1)
       end else Raw_empty
  | 8 | 9 ->
     let r1 = random_regex (depth-1) look false in
     (* not generating regexes with groups inside lookbehinds *)
     let l = random_look() in
     Raw_lookaround(l, r1)
  | _ -> failwith "random range error"


let random_raw () : raw_regex =
  let max = Random.int max_depth in
  random_regex max true true

(** * Creating Random Strings  *)

let random_string () : string =
  let size = (Random.int max_string) in
  String.init size (fun _ -> random_char())
  
  
(** * The differential fuzzer itself  *)
let fuzzer () : unit =

  (* adding some statistics over nullable + *)
  let total_nn = ref 0 in
  let total_cdn = ref 0 in
  let total_cin = ref 0 in
  let total_lnn = ref 0 in
  let total_ln = ref 0 in
  let total_timeout = ref 0 in
  
  for i = 0 to max_tests do 
    let raw = random_raw() in
    let str = random_string() in
    let comp = compare_engines raw str in
    if (not comp) then total_timeout := !total_timeout +1;

    let (nn,cdn,cin,lnn,ln) = plus_stats (annotate raw) in
    total_nn := !total_nn + nn;
    total_cdn := !total_cdn + cdn;
    total_cin := !total_cin + cin;
    total_lnn := !total_lnn + lnn;
    total_ln := !total_ln + ln;

  done;

  Printf.printf "\n=====\n\n";
  Printf.printf "Total tests:                 %d\n" max_tests;
  Printf.printf "NonNullable+:                %d\n" !total_nn;
  Printf.printf "ContextDependentNullable+:   %d\n" !total_cdn;
  Printf.printf "ContextIndependentNullable+: %d\n" !total_cin;
  Printf.printf "NonNullableLazy+:            %d\n" !total_lnn;
  Printf.printf "NullableLazy+:               %d\n" !total_ln;
  Printf.printf "JS Backtracking Timeouts:    %d\n" !total_timeout
