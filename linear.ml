(** * Linear VM-based matcher for Regexes with Capture Groups and Lookarounds *)
(* In this version, we only have lookbehinds without capture groups indide lookbehinds *)

open Oracle
open Bytecode
open Regex
open Compiler
open Interpreter


(* assumes that r is a valid regex *)
(* we don't have any groups to reconstruct since lookbehinds don't have groups in them *)
let match_with_lookbehinds ?(verbose=true) ?(debug=false) (r:regex) (str:string) : cap_regs option =
  let full_regex = lazy_prefix r in (* adding a lazy prefix to the main expression *)
  let (full_code, entries) = compile_with_lookbehinds full_regex in
  let full_result = interp_default_init ~verbose ~debug r full_code str (max_lookaround r) Forward entries in
  match full_result with
  | None -> None
  | Some thread ->
     let regs = ref thread.regs in
     filter_capture r regs thread.cap_clk thread.look_clk thread.quants (-1); (* filtering old values *)
     Some !regs
   
  
let full_match ?(verbose=true) ?(debug=false) (raw:raw_regex) (str:string) : cap_regs option =
  assert (raw_regex_valid raw);
  let re = annotate raw in
  let ca = match_with_lookbehinds ~verbose ~debug re str in
  if verbose then
    Printf.printf "%s\n" (print_result re str ca);
  ca
            
let get_linear_result ?(verbose=false) ?(debug=false) (raw:raw_regex) (str:string) : string =
  let capop = full_match ~verbose ~debug raw str in
  print_result ~verbose (annotate raw) str capop
