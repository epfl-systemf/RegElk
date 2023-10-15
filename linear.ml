(** * Linear VM-based matcher for Regexes with Capture Groups and Lookarounds *)

open Oracle
open Bytecode
open Regex
open Compiler
open Interpreter
open Cdn
open Flags

(** * Bulding the Oracle  *)

   
(* we consider lookarounds by reverse order of their identifiers *)
(* we do not need to do this for the main regex *)
let build_oracle (cr:compiled_regex) (str:string): oracle =
  let max = max_lookaround cr.ast in
  let o = create_oracle (String.length str) (max + 1) in
  for lid = max downto 1 do
    let bytecode = cr.build_look.(lid) in
    let looktype = cr.looktypes.(lid) in
    let direction = oracle_direction looktype in
    let lookcdn = cr.lookcdns.(lid) in
    let lookast = cr.lookast.(lid) in
    (* the interface should be changed for more clarity *)
    let newcr : compiled_regex =
      { ast=lookast; main=bytecode; looktypes=cr.looktypes; lookcdns=cr.lookcdns; lookast=cr.lookast;
        build_look=cr.build_look; capture_look=cr.capture_look; plus_code=cr.plus_code} in
    ignore (interp_default_init newcr str o direction lookcdn)
           (* we don't want the return value, we just want to write to the oracle *)
  done;
  o                             (* returning the modified oracle *)


(** * Building Capture Groups  *)

(* negative lookarounds don't capture anything *)
let capture_type (l:lookaround) : bool =
  match l with
  | Lookahead | Lookbehind -> true
  | NegLookahead | NegLookbehind -> false
                                

(* returns the register array if there is a match *)
let build_capture (cr:compiled_regex) (str:string) (o:oracle): (int Array.t) option =
  let max_look = max_lookaround cr.ast in
  let max_cap = max_group cr.ast in
  let max_quant = max_quant cr.ast in
  let capture = Interpreter.Regs.init_regs(2*max_cap+2) in
  let look = Interpreter.Regs.init_regs (max_look+1) in
  let quant = Interpreter.Regs.init_regs (max_quant+1) in
  let main_bytecode = cr.main in
  let main_cdn = compile_cdns cr.ast in
  let main_result = interp cr str o Forward 0 capture look quant 0 main_cdn in
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
          let looktype = cr.looktypes.(lid) in
          if (capture_type looktype) then (* not for negative lookarounds *)
            let bytecode = cr.capture_look.(lid) in
            let direction = capture_direction looktype in
            let lookcdn = cr.lookcdns.(lid) in
            let lookast = cr.lookast.(lid) in
            (* the interface should be changed for more clarity *)
            let newcr : compiled_regex =
              { ast=lookast; main=bytecode; looktypes=cr.looktypes; lookcdns=cr.lookcdns; lookast=cr.lookast;
                build_look=cr.build_look; capture_look=cr.capture_look; plus_code=cr.plus_code} in
            let result = interp newcr str o direction cp !capture !look !quant 0 lookcdn in
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
     let match_capture = filter_reset cr.ast !capture !look !quant (-1) in (* filtering old values *)
     if !debug then Printf.printf "filtered regs: %s\n%!" (debug_regs match_capture);
     Some (match_capture)
     

let matcher (cr:compiled_regex) (str:string) : (int Array.t) option =
  let o = build_oracle cr str in
  if !debug then
    Printf.printf "%s\n" (print_oracle o);
  let ca = build_capture cr str o in
  if !verbose then
    Printf.printf "%s\n" (print_result cr.ast str ca);
  ca
     
let full_match (raw:raw_regex) (str:string) : (int Array.t) option =
  let re = annotate raw in
  let cr = full_compilation re in
  matcher cr str
              
let get_linear_result (raw:raw_regex) (str:string) : string =
  let capop = full_match raw str in
  print_result (annotate raw) str capop
