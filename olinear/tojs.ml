(** * Building JS programs  *)
(* so that we can compare our algorithms to a JS engine *)

open Regex
open Sys
open Filename
open Linear

(** * JS Regex pretty-printing  *)
(* printing regexes in the JS style so that we can compare our results to a JS engine *)
(* adding a non-capturing group to a string *)
let noncap (s:string) : string =
  "(?:" ^ s ^ ")"

(* we put non-capturing groups everywhere to ensure the non-ambiguity *)
(* we could be more clever and put less non-capturing groups *)
let rec print_js (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_char ch -> String.make 1 ch
  | Raw_dot -> "."
  | Raw_alt (r1, r2) -> noncap(print_js r1) ^ "|" ^ noncap(print_js r2)
  | Raw_con (r1, r2) -> noncap(print_js r1) ^ noncap(print_js r2)
  | Raw_quant (q, r1) -> noncap(print_js r1) ^ print_quant q
  | Raw_capture r1 -> "(" ^ print_js r1 ^ ")"
  | Raw_lookaround (l, r1) -> "(" ^ print_lookaround l ^ print_js r1 ^ ")"


(** * Calling the JS Matcher  *)

(* geting the result of a command as a strng *)
let string_of_command (command:string) : string =
 let tmp_file = Filename.temp_file "" ".txt" in
 let _ = Sys.command @@ command ^ " >" ^ tmp_file in
 let chan = open_in tmp_file in
 let output = ref "" in
 try
   while true do
     output := !output ^ input_line chan ^ "\n"
   done; !output
 with
   End_of_file ->
   close_in chan;
   !output


(* getting its result as a string *)
let get_js_result (raw:raw_regex) (str:string) : string =
  let js_regex = print_js raw in
  let js_regex = "'" ^ js_regex ^ "'" in (* adding quotes to escape special characters *)
  let js_command = "node jsmatcher.js " ^ js_regex ^ " " ^ str in
  string_of_command(js_command)

  
(** *  Comparing JS engine with our engine *)
  
let compare_engines ?(verbose=false) ?(debug=false) (raw:raw_regex) (str:string) : unit =
  Printf.printf "\027[36mRegex:\027[0m %s || " (print_regex (annotate raw));
  Printf.printf "\027[36mJS Regex:\027[0m %s || " (print_js raw);
  Printf.printf "\027[36mString:\027[0m %s\n\n%!" str;
  let sjs = get_js_result raw str in
  Printf.printf "\027[35mJS result:\027[0m\n%s%!" sjs;
  let sl = get_linear_result ~verbose ~debug raw str in
  Printf.printf "\027[35mLinear result:\027[0m\n%s%!" sl;
  assert (String.compare sjs sl = 0)
