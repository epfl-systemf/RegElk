(** * Bytecode Compiler  *)
(* compile an (annotated) regex to bytecode *)

open Regex
open Bytecode


(** * Registers *)
   
(* defining registers corresponding to a given capture group *)
let start_reg (c:capture) : register = 2 * c
let end_reg (c:capture) : register = (2*c) + 1

(* Clearing Start Registers *)
(* Generating code to clear the start registers inside a quantifier to comply with the JS semantics *)
let rec clear_regs (cstart:capture) (nb:int) : instruction list =
  assert (nb >= 0);
  if (nb = 0) then []
  else (ClearRegister (start_reg (cstart+nb-1)))::(clear_regs cstart (nb-1))

(* Clear the interval [cstart; cend[ *)
let clear_range (cstart:capture) (cend:capture) : instruction list =
  clear_regs cstart (cend - cstart)

(** * Lookaround Memory  *)

let rec clear_looks (lstart:lookid) (nb:int) : instruction list =
  assert (nb >= 0);
  if (nb = 0) then []
  else (ClearMemory (lstart+nb-1))::(clear_looks lstart (nb-1))

let clear_mem (lstart:lookid) (lend:lookid) : instruction list =
  clear_looks lstart (lend - lstart)
  
  
(** * Compilation  *)
  (* currently, we are quadratic because of list concatenation *)
  (* our clearing of the registers and memory is also not optimal and can result in quadratic bytecode *)
  (* experimental V8 has the same issue *)

  
(* Recursively compiles a regex *)
(* [fresh] is the next available instruction label *)
(* also returns the next fresh label after compilation *)
let rec compile (r:regex) (fresh:label) : instruction list * label =
  match r with
  | Re_empty -> ([], fresh)
  | Re_char ch -> ([Consume ch], fresh+1)
  | Re_dot -> ([ConsumeAll], fresh+1)
  | Re_con (r1, r2) ->
     let (l1, f1) = compile r1 fresh in
     let (l2, f2) = compile r2 f1 in
     (l1 @ l2, f2)
  | Re_alt (r1, r2) ->
     let (l1, f1) = compile r1 (fresh+1) in
     let (l2, f2) = compile r2 (f1+1) in
     ([Fork (fresh+1, f1+1)] @ l1 @ [Jmp f2] @ l2, f2)
  | Re_quant (nul, qid, quant, r1) ->
     begin match quant with
     | Star ->
        if nul then 
          let (l1, f1) = compile r1 (fresh+3) in
          ([Fork (fresh+1, f1+1); BeginLoop; SetQuantToClock qid] @ l1 @ [EndLoop; Jmp fresh], f1+2)
        else
          let (l1, f1) = compile r1 (fresh+2) in
          ([Fork (fresh+1, f1+1); SetQuantToClock qid] @ l1 @ [Jmp fresh], f1+1)
     | LazyStar ->
        if nul then 
          let (l1, f1) = compile r1 (fresh+3) in
          ([Fork (f1+1, fresh+1); BeginLoop; SetQuantToClock qid] @ l1 @ [EndLoop; Jmp fresh], f1+2)
        else
          let (l1, f1) = compile r1 (fresh+2) in
          ([Fork (f1+1, fresh+1); SetQuantToClock qid] @ l1 @ [Jmp fresh], f1+1)
     | Plus ->
        if nul then
          (* Compiling as a concatenation *)
          let (l1, f1) = compile (Re_con(r1,Re_quant(nul,qid,Star,r1))) (fresh+1) in
          ([SetQuantToClock qid] @ l1, f1)
        else
          let (l1, f1) = compile r1 (fresh+1) in
          ([SetQuantToClock qid] @ l1 @ [Fork (fresh,f1+1)], f1+1)
     | LazyPlus ->
        if nul then
          (* Compiling as a concatenation *)
          let (l1, f1) = compile (Re_con(r1,Re_quant(nul,qid,LazyStar,r1))) (fresh+1) in
          ([SetQuantToClock qid] @ l1, f1)
        else
          let (l1, f1) = compile r1 (fresh+1) in
          ([SetQuantToClock qid] @ l1 @ [Fork (f1+1,fresh)], f1+1)
     end
  | Re_capture (cid, r1) ->
     let (l1, f1) = compile r1 (fresh+1) in
     ([SetRegisterToCP (start_reg cid)] @ l1 @ [SetRegisterToCP (end_reg cid)], f1+1)
  | Re_lookaround (lookid, looktype, r1) ->
     (* does not compile the lookarounds it depends on, only the current expression *)
     begin match looktype with
     | Lookahead | Lookbehind -> ([CheckOracle lookid], fresh+1)
     | NegLookahead | NegLookbehind -> ([NegCheckOracle lookid], fresh+1)
     end

(* adds an accept at the end of the bytecode *)
(* And a lazy star at the beginning *)
let compile_to_bytecode (r:regex) : code =
  let (c,_) = compile r 0 in
  let full_c = c @ [Accept] in
  Array.of_list full_c

(* same but with a WriteOracle instruction instead of an Accept *)
(* l is the current lookid we are compiling the regex for *)
let compile_to_write (r:regex) (l:lookid): code =
  let (c,_) = compile r 0 in
  let full_c = c @ [WriteOracle l] in
  Array.of_list full_c
