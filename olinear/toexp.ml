(** * Building JS programs for the Experimental Engine *)
(* so that we can compare our algorithms to another linear JS engine *)

open Regex
open Sys
open Filename
open Linear
open Tojs

(** * Experimental Regex pretty-printing  *)
(* printing regexes in the JS style so that we can compare our results to a JS engine *)
(* adding a non-capturing group to a string *)
let noncap (s:string) : string =
  "(?:" ^ s ^ ")"

(* we put non-capturing groups everywhere to ensure the non-ambiguity *)
(* we could be more clever and put less non-capturing groups *)
let rec print_exp (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_char ch -> String.make 1 ch
  | Raw_dot -> "."
  | Raw_alt (r1, r2) -> noncap(print_exp r1) ^ "|" ^ noncap(print_exp r2)
  | Raw_con (r1, r2) -> noncap(print_exp r1) ^ noncap(print_exp r2)
  | Raw_quant (q, r1) -> noncap(print_exp r1) ^ print_quant q
  | Raw_capture r1 -> "(" ^ print_exp r1 ^ ")"
  | Raw_lookaround (l, r1) -> failwith "Experimental does not support lookarounds"


(** * Calling the JS Experimental Matcher  *)
(* for this we need to call d8 with a particular argument to enable experimental *)


(* getting its result as a string *)
let get_experimental_result (raw:raw_regex) (str:string) : string =
  let js_regex = print_exp raw in
  let js_regex = "'" ^ js_regex ^ "'" in (* adding quotes to escape special characters *)
  let js_string = "'"^str^"'" in
  let js_command = "~/v8/v8/out/x64.release/d8 --enable-experimental-regexp-engine expmatcher.js -- " ^ js_regex ^ " " ^ js_string in
  string_of_command(js_command)
