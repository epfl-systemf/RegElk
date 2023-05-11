(** * Linear VM-based matcher for Regexes with Capture Groups and Lookarounds *)

open Oracle
open Bytecode
open Regex
open Compiler
open Interpreter


(** * Bulding the Oracle  *)

(* when building the oracle, we compile lookarounds in the reverse direction (forward for lookbehind) *)
let reverse_type (l:lookaround) (r:regex) : regex =
  match l with
  | Lookahead | NegLookahead -> reverse_regex r
  | Lookbehind | NegLookbehind -> r


   
(* we consider lookarounds by reverse order of their identifiers *)
(* we do not need to do this for the main regex *)
let build_oracle ?(verbose = true) ?(debug=false) (r:regex) (str:string): oracle =
  let max = max_lookaround r in
  let o = create_oracle (String.length str) (max + 1) in
  for lid = max downto 1 do
    let (reg, looktype) = get_look r lid in
    let lookreg_nc = remove_capture reg in (* no capture *)
    let lookreg_rev = reverse_type looktype lookreg_nc in (* correct direction *)
    let lookreg = lazy_prefix lookreg_rev in              (* lazy star prefix *)
    let bytecode = compile_to_write lookreg lid in
    let direction = oracle_direction looktype in 
    ignore (matcher_interp ~verbose ~debug bytecode str o direction)
           (* we don't want the return value, we just want to write to the oracle *)
  done;
  o                             (* returning the modified oracle *)


(** * Building Capture Groups  *)
  
    (* todo *)
