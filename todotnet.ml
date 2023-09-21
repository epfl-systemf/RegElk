(** * Building .NET7 programs  *)

open Regex
open Linear
open Tojs
open Charclasses

(** * .NET Regex pretty-printing  *)
   
(* adding a non-capturing group to a string *)
let noncap (s:string) : string =
  "(?:" ^ s ^ ")"

(* we put non-capturing groups everywhere to ensure the non-ambiguity *)
(* we could be more clever and put less non-capturing groups *)
let rec print_dotnet (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_character c -> print_character c
  | Raw_alt (r1, r2) -> noncap(print_dotnet r1) ^ "|" ^ noncap(print_dotnet r2)
  | Raw_con (r1, r2) -> noncap(print_dotnet r1) ^ noncap(print_dotnet r2)
  | Raw_quant (q, r1) -> noncap(print_dotnet r1) ^ print_quant q
  | Raw_count (q, r1) -> noncap(print_dotnet r1) ^ print_counted_quant q
  | Raw_capture r1 -> "(" ^ print_dotnet r1 ^ ")"
  | Raw_lookaround (l, r1) -> "(" ^ print_lookaround l ^ print_dotnet r1 ^ ")"
  | Raw_anchor a -> print_anchor a 
   
(** * Calling Dotnet and getting it's result  *)
(* when linear is true, tries to use the non-backtracking engine (which doe snot support lookarounds) *)
                  
let get_dotnet_result (linear:bool) (raw:raw_regex) (str:string) : string =
  let linear_str = if linear then " linear " else " bt " in
  let dotnet_regex = "'" ^ print_dotnet raw ^ "'" in (* quotes for special characters *)
  let dotnet_string = "'" ^ str ^ "'" in 
  let dotnet_command = "dotnet run --project dotnet_matcher/ " ^ linear_str ^ " matcher " ^ dotnet_regex ^ dotnet_string in
  string_of_command(dotnet_command)


let get_time_dotnet (linear:bool) (raw:raw_regex) (str:string) : string =
  let linear_str = if linear then " linear " else " bt " in
  let dotnet_regex = " '" ^ print_dotnet raw ^ "' " in (* quotes for special characters *)
  let dotnet_string = " '" ^ str ^ "' " in 
  let dotnet_command = "dotnet run --project dotnet_matcher/ " ^ linear_str ^ " timer " ^ dotnet_regex ^ dotnet_string in
  string_of_command(dotnet_command)

    
