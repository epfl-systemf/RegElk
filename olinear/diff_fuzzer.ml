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
  match (Random.int 4) with
  | 0 -> Lookahead
  | 1 -> NegLookahead
  | 2 -> Lookbehind
  | 3 -> NegLookbehind
  | _ -> failwith "random range error"

(* with a maximal number of recursion [depth] *)
let rec random_regex (depth:int) : raw_regex =
  let rand = if (depth=0) then Random.int 3 else Random.int 10 in
  match rand with
  | 0 -> Raw_empty
  | 1 -> let x = random_char() in Raw_char x
  | 2 -> Raw_dot
  | 3 ->
     let r1 = random_regex (depth-1) in
     let r2 = random_regex (depth-1) in
     Raw_alt (r1, r2)
  | 4 ->
     let r1 = random_regex (depth-1) in
     let r2 = random_regex (depth-1) in
     Raw_con (r1, r2)
  | 5 ->
     let r1 = random_regex (depth-1) in
     let q = random_quant() in
     Raw_quant(q, r1)
  | 6 | 7 ->
     let r1 = random_regex (depth-1) in
     Raw_capture(r1)
  | 8 | 9 ->
     let r1 = random_regex (depth-1) in
     let l = random_look() in
     Raw_lookaround(l, r1)
  | _ -> failwith "random range error"


let random_raw () : raw_regex =
  let max = Random.int max_depth in
  random_regex (max)

(** * Creating Random Strings  *)

let random_string () : string =
  let size = (Random.int max_string) in
  String.init size (fun _ -> random_char())
  
  
(** * The differential fuzzer itself  *)
let fuzzer () : unit =
  for i = 0 to max_tests do 
    let raw = random_raw() in
    let str = random_string() in
    compare_engines raw str
  done
    
  
