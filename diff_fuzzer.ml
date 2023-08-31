(** * Differential Fuzzer  *)
(* randomly generating regexes and strings and comparing the output of our algorithm VS the JS matcher *)

open Linear
open Tojs
open Regex
open Random
open Charclasses

let _ = Random.self_init()


      
(* we restrict ourselves to a small alphabet *)
let alphabet = ['a'; 'b'; '-']
(* with a dash (non-ascii) to test word boundaries *)

let max_depth = 50

let max_string = 100

let max_tests = 1000

let max_counted = 10

let max_class = 20

(** * Creating Random Regexes  *)
   
let random_char () : char =
  let idx = Random.int (List.length alphabet) in
  List.nth alphabet idx

let random_quant () : quantifier =
  match (Random.int 6) with
  | 0 -> Star
  | 1 -> LazyStar
  | 2 -> Plus
  | 3 -> LazyPlus
  | 4 -> QuestionMark
  | 5 -> LazyQuestionMark
  | _ -> failwith "random range error"
       
let random_counted_quant () : counted_quantifier =
  let min = Random.int max_counted in
  let greedy = Random.bool () in
  let max = if Random.bool() then None else Some (min + Random.int max_counted) in
  { min=min; max=max; greedy=greedy }
          

let random_look () : lookaround =
  match (Random.int 4) with
  | 0 -> Lookahead
  | 1 -> NegLookahead
  | 2 -> Lookbehind
  | 3 -> NegLookbehind
  | _ -> failwith "random range error"

let random_anchor () : anchor =
  match (Random.int 4) with
  | 0 -> BeginInput
  | 1 -> EndInput
  | 2 -> WordBoundary
  | 3 -> NonWordBoundary
  | _ -> failwith "random range error"

let random_group () : char_group =
  match (Random.int 6) with
  | 0 -> Digit
  | 1 -> NonDigit
  | 2 -> Word
  | 3 -> NonWord
  | 4 -> Space
  | 5 -> NonSpace
  | _ -> failwith "random range error"

let random_range () : char * char =
  let minc = Random.int 256 in
  let offset = Random.int (256-minc) in
  (char_of_int minc, char_of_int (minc+offset))

let random_elt () : char_class_elt =
  match (Random.int 3) with
  | 0 -> let x = random_char() in CChar x
  | 1 -> let r = random_range() in CRange (fst r, snd r)
  | 2 -> let g = random_group() in CGroup g
  | _ -> failwith "random range error"

let random_class () : char_class =
  let size = Random.int max_class in
  List.init size (fun _ -> random_elt())

(* with a maximal number of recursion [depth] *)
(* the [look] boolean specifies if lookarounds are allowed *)
let rec random_regex (depth:int) (look:bool): raw_regex =
  let max = if look then 15 else 13 in
  let rand = if (depth=0) then Random.int 3 else Random.int max in
  match rand with
  | 0 -> Raw_empty
  | 1 -> let x = random_char() in Raw_char x
  | 2 -> Raw_dot
  | 3 -> let g = random_group () in Raw_group g
  | 4 -> let cl = random_class() in Raw_class cl
  | 5 -> let cl = random_class() in Raw_neg_class cl
  | 6 -> let a = random_anchor() in Raw_anchor a
  | 7 ->
     let r1 = random_regex (depth-1) look in
     let r2 = random_regex (depth-1) look in
     Raw_alt (r1, r2)
  | 8 ->
     let r1 = random_regex (depth-1) look in
     let r2 = random_regex (depth-1) look in
     Raw_con (r1, r2)
  | 9 ->
     let r1 = random_regex (depth-1) look in
     let q = random_quant() in
     Raw_quant(q, r1)
  | 10 ->
     let r1 = random_regex (depth-1) look in
     let q = random_counted_quant() in
     Raw_count(q, r1)
  | 11 | 12 ->
     let r1 = random_regex (depth-1) look in
     Raw_capture(r1)
  | 13 | 14 ->
     let r1 = random_regex (depth-1) look in
     let l = random_look() in
     Raw_lookaround(l, r1)
  | _ -> failwith "random range error"


let random_raw () : raw_regex =
  let max = Random.int max_depth in
  random_regex max true

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
