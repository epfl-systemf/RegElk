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
let rec clear_regs (cstart:capture) (nb:int) : code =
  assert (nb >= 0);
  if (nb = 0) then []
  else (ClearRegister (start_reg (cstart+nb-1)))::(clear_regs cstart (nb-1))

(* Clear the interval [cstart; cend[ *)
let clear_range (cstart:capture) (cend:capture) : code =
  clear_regs cstart (cend - cstart)

  
(** * Compilation  *)
  
(* Recursively compiles a regex *)
(* [fresh] is the next available instruction label *)
(* also returns the next fresh label after compilation *)
let rec compile (r:regex) (fresh:label) : code * label =
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
  | Re_quant (cstart, cend, quant, r1) ->
     let range = cend - cstart in
     begin match quant with
     | Star ->
        let (l1, f1) = compile r1 (fresh+1+range) in
        ([Fork (fresh+1, f1+1)] @ clear_range cstart cend @ l1 @ [Jmp fresh], f1+1)
     | LazyStar ->
        let (l1, f1) = compile r1 (fresh+1+range) in
        ([Fork (f1+1, fresh+1)] @ clear_range cstart cend @ l1 @ [Jmp fresh], f1+1)
     | Plus ->
        let (l1, f1) = compile r1 fresh in (* not clearing registers on the first iteration *)
        (l1 @ [Fork (f1+1, f1+2+range)] @ clear_range cstart cend @ [Jmp fresh], f1+2+range )
     | LazyPlus ->
        let (l1, f1) = compile r1 fresh in (* not clearing registers on the first iteration *)
        (l1 @ [Fork (f1+2+range, f1+1)] @ clear_range cstart cend @ [Jmp fresh], f1+2+range )
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
  c @ [Accept]

(* same but with a WriteOracle instruction instead of an Accept *)
(* l is the current lookid we are compiling the regex for *)
let compile_to_write (r:regex) (l:lookid): code =
  let (c,_) = compile r 0 in
  c @ [WriteOracle l]
        (* TODO: I'm probably forgetting a lazy star at the beginning *)
