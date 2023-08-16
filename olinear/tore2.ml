(** * Building RE2 programs  *)
(* Using the Re2 OCaml wrapper from JaneStreet *)

open Regex
open Linear
open Re2
open Unix
open Gc

(** * RE2 Regex pretty-printing  *)
(* printing regexes in the RE2 style so that we can compare our results to RE2 *)
   
(* adding a non-capturing group to a string *)
let noncap (s:string) : string =
  "(?:" ^ s ^ ")"

(* we put non-capturing groups everywhere to ensure the non-ambiguity *)
(* we could be more clever and put less non-capturing groups *)
let rec print_re2 (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_char ch -> String.make 1 ch
  | Raw_dot -> "."
  | Raw_alt (r1, r2) -> noncap(print_re2 r1) ^ "|" ^ noncap(print_re2 r2)
  | Raw_con (r1, r2) -> noncap(print_re2 r1) ^ noncap(print_re2 r2)
  | Raw_quant (q, r1) -> noncap(print_re2 r1) ^ print_quant q
  | Raw_capture r1 -> "(" ^ print_re2 r1 ^ ")"
  | Raw_lookaround (l, r1) -> failwith "RE2 does not support lookarounds"
  | Raw_anchor a -> print_anchor a (* RE2 has the same syntax if we don't activate the multiline or unicode flags *)
   
(** * Calling RE2 and getting it's result  *)
                            
let print_res_op (o:string option) : string =
  match o with
  | None -> "Undefined"
  | Some str -> str

(* printing the result of a match *)
let print_re2_result res :  string =
  match res with
  | Error _ -> "NoMatch\n"
  | Ok result -> 
    let out = ref "" in
    for i = 0 to (Array.length result) - 1 do
      out := !out ^ "#" ^ string_of_int i ^ ":" ^ print_res_op (Array.get result i) ^ "\n"
    done;
    !out ^ "\n"
     
   
let get_re2_result (raw:raw_regex) (str:string) : string =
  let re = Re2.create_exn (print_re2 raw) in
  let result = Re2.find_submatches re str in
  print_re2_result result

let get_time_re2 (raw:raw_regex) (str:string) : float =
  let re = Re2.create_exn (print_re2 raw) in
  Gc.full_major();
  let tstart = Unix.gettimeofday() in
  ignore(Re2.find_submatches re str);
  let tend = Unix.gettimeofday() in
  tend -. tstart
