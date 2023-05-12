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
let build_oracle ?(verbose=true) ?(debug=false) (r:regex) (str:string): oracle =
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
                                
  
let build_capture ?(verbose=true) ?(debug=false) (r:regex) (str:string) (o:oracle): cap_regs option =
  let max = max_lookaround r in
  let mem = init_mem() in
  let regs = init_regs() in
  let main_regex = lazy_prefix r in (* lazy star prefix, only for the main expression *)
  let main_bytecode = compile_to_bytecode main_regex in
  let main_result = interp ~verbose ~debug main_bytecode str o Forward 0 regs mem in
  match main_result with
  | None -> None
  | Some thread ->              (* we have a match and want to rebuild capture groups *)
     let mem = ref thread.mem in
     let regs = ref thread.regs in
     for lid=1 to max do
       match (get_mem !mem lid) with
       | None -> ()             (* the lookaround wasn't needed in the match *)
       | Some cp ->             (* the lookaround had a match at cp *)
          let (reg, looktype) = get_look r lid in
          if (capture_type looktype) then (* not for negative lookarounds *)
            let lookreg = capture_regex looktype reg in
            let bytecode = compile_to_bytecode lookreg in
            let direction = capture_direction looktype in
            let result = interp ~verbose ~debug bytecode str o direction cp !regs !mem in
            begin match result with
            | None -> failwith "result expected from the oracle"
            | Some t ->
               mem := t.mem;    (* updating the lookaround memory *)
               regs := t.regs   (* updating the capture regs *)
            end
     done;
     Some (!regs)
     
  
let full_match ?(verbose=true) ?(debug=false) (raw:raw_regex) (str:string) : cap_regs option =
  let re = annotate raw in
  let o = build_oracle ~verbose ~debug re str in
  if debug then
    Printf.printf "%s\n" (print_oracle o);
  let ca = build_capture ~verbose ~debug re str o in
  if verbose then
    Printf.printf "%s\n" (print_result re str ca);
  ca
            
let get_linear_result (raw:raw_regex) (str:string) : string =
  let capop = full_match ~verbose:false ~debug:false raw str in
  print_result ~verbose:false (annotate raw) str capop
