(** * Building Rust programs  *)
(* using the cargos rust_matcher and rust_timer *)

open Regex
open Linear
open Tojs


(** * Rust Regex pretty-printing  *)
(* printing regexes in the Rust style so that we can compare our results to Rust *)
   
(* adding a non-capturing group to a string *)
let noncap (s:string) : string =
  "(?:" ^ s ^ ")"

(* we put non-capturing groups everywhere to ensure the non-ambiguity *)
(* we could be more clever and put less non-capturing groups *)
let rec print_rust (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_char ch -> String.make 1 ch
  | Raw_dot -> "."
  | Raw_alt (r1, r2) -> noncap(print_rust r1) ^ "|" ^ noncap(print_rust r2)
  | Raw_con (r1, r2) -> noncap(print_rust r1) ^ noncap(print_rust r2)
  | Raw_quant (q, r1) -> noncap(print_rust r1) ^ print_quant q
  | Raw_count (q, r1) -> noncap(print_rust r1) ^ print_counted_quant q
  | Raw_capture r1 -> "(" ^ print_rust r1 ^ ")"
  | Raw_lookaround (l, r1) -> failwith "Rust does not support lookarounds"
  | Raw_anchor a -> print_anchor a (* Rust has the same anchor syntax *)
   
(** * Calling Rust and getting it's result  *)

let get_rust_result (raw:raw_regex) (str:string) : string =
  let rust_regex = print_rust raw in
  let rust_regex = "'" ^ rust_regex ^ "'" in (* quotes for special characters *)
  let rust_command = "cargo run --manifest-path rust_matcher/Cargo.toml --quiet '" ^ str ^ "' " ^ rust_regex in
  string_of_command(rust_command)


let get_time_rust (raw:raw_regex) (str:string) : string =
  let rust_regex = print_rust raw in
  let rust_regex = "'" ^ rust_regex ^ "'" in (* quotes for special characters *)
  let rust_command = "cargo run --manifest-path rust_timer/Cargo.toml --quiet '" ^ str ^ "' " ^ rust_regex in
  string_of_command(rust_command)

    
