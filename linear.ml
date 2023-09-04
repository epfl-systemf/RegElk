(** * Linear VM-based matcher for Regexes with Capture Groups and Lookarounds *)

open Oracle
open Bytecode
open Regex
open Compiler
open Interpreter
open Cdn
open Flags

(** * Bulding the Oracle  *)

(* when building the oracle, we compile lookarounds in the reverse direction (forward for lookbehind) *)
let reverse_type (l:lookaround) (r:regex) : regex =
  match l with
  | Lookahead | NegLookahead -> reverse_regex r
  | Lookbehind | NegLookbehind -> r

   
(* we consider lookarounds by reverse order of their identifiers *)
(* we do not need to do this for the main regex *)
let build_oracle (r:regex) (str:string): oracle =
  let max = max_lookaround r in
  let o = create_oracle (String.length str) (max + 1) in
  for lid = max downto 1 do
    let (reg, looktype) = get_look r lid in
    let lookreg_nc = remove_capture reg in (* no capture *)
    let lookreg_rev = reverse_type looktype lookreg_nc in (* correct direction *)
    let lookreg = lazy_prefix lookreg_rev in              (* lazy star prefix *)
    let bytecode = compile_to_write lookreg lid in
    let direction = oracle_direction looktype in
    let lookcdn = compile_cdns lookreg in
    ignore (interp_default_init lookreg bytecode str o direction lookcdn)
           (* we don't want the return value, we just want to write to the oracle *)
  done;
  o                             (* returning the modified oracle *)


(** * Building Capture Groups  *)

(* when building capture groups, we compile lookarounds in the expected direction *)
let capture_regex (l:lookaround) (r:regex) : regex =
  match l with
  | Lookahead | NegLookahead -> r
  | Lookbehind | NegLookbehind -> reverse_regex r

(* negative lookarounds don't capture anything *)
let capture_type (l:lookaround) : bool =
  match l with
  | Lookahead | Lookbehind -> true
  | NegLookahead | NegLookbehind -> false
                                

(* returns the register array if there is a match *)
let build_capture (r:regex) (str:string) (o:oracle): (int Array.t) option =
  let max_look = max_lookaround r in
  let max_cap = max_group r in
  let max_quant = max_quant r in
  let capture = Interpreter.Regs.init_regs(2*max_cap+2) in
  let look = Interpreter.Regs.init_regs (max_look+1) in
  let quant = Interpreter.Regs.init_regs (max_quant+1) in
  
  let main_regex = lazy_prefix r in (* lazy star prefix, only for the main expression *)
  let main_bytecode = compile_to_bytecode main_regex in
  let main_cdn = compile_cdns main_regex in
  let main_result = interp r main_bytecode str o Forward 0 capture look quant 0 main_cdn in
  match main_result with
  | None -> None
  | Some thread ->              (* we have a match and want to rebuild capture groups *)
     let capture = ref thread.capture_regs in
     let look = ref thread.look_regs in
     let quant = ref thread.quant_regs in
     for lid=1 to max_look do
       match (Interpreter.Regs.get_cp !look lid) with
       | None -> ()             (* the lookaround wasn't needed in the match *)
       | Some cp ->             (* the lookaround had a match at cp *)
          let (reg, looktype) = get_look r lid in
          if (capture_type looktype) then (* not for negative lookarounds *)
            let lookreg = capture_regex looktype reg in
            let bytecode = compile_to_bytecode lookreg in
            let direction = capture_direction looktype in
            let lookcdn = compile_cdns lookreg in
            let result = interp lookreg bytecode str o direction cp !capture !look !quant 0 lookcdn in
            begin match result with
            | None -> failwith "result expected from the oracle"
            | Some t ->
               (* updating all registers *)
               capture := t.capture_regs;
               look := t.look_regs;
               quant := t.quant_regs
            end
     done;
     if !debug then Printf.printf "regs: %s\n%!" (Interpreter.Regs.to_string !capture);
     let match_capture = filter_reset r !capture !look !quant (-1) in (* filtering old values *)
     if !debug then Printf.printf "filtered regs: %s\n%!" (debug_regs match_capture);
     Some (match_capture)
     
  
let full_match (raw:raw_regex) (str:string) : (int Array.t) option =
  let re = annotate raw in
  let o = build_oracle re str in
  if !debug then
    Printf.printf "%s\n" (print_oracle o);
  let ca = build_capture re str o in
  if !verbose then
    Printf.printf "%s\n" (print_result re str ca);
  ca
            
let get_linear_result (raw:raw_regex) (str:string) : string =
  let capop = full_match raw str in
  print_result (annotate raw) str capop
