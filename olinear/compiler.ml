(** * Bytecode Compiler  *)
(* compile an (annotated) regex to bytecode *)

open Regex
open Bytecode


(** * Registers *)
   
(* defining registers corresponding to a given capture group *)
let start_reg (c:capture) : register = 2 * c
let end_reg (c:capture) : register = (2*c) + 1
  
  
(** * Compilation  *)
  (* currently, we are quadratic because of list concatenation *)
  (* our clearing of the registers and memory is also not optimal and can result in quadratic bytecode *)
  (* experimental V8 has the same issue *)

  
(* Recursively compiles a regex *)
(* [fresh] is the next available instruction label *)
(* also returns the next fresh label after compilation *)
(* if the option progress is false, then this generates code that cannot make progress in the string *)
let rec compile (r:regex) (fresh:label) (progress:bool) : instruction list * label =
  match r with
  | Re_empty -> ([], fresh)
  | Re_char ch ->
     if progress then ([Consume ch], fresh+1)
     else ([Fail], fresh+1)
  | Re_dot ->
     if progress then ([ConsumeAll], fresh+1)
     else ([Fail], fresh+1)
  | Re_con (r1, r2) ->
     let (l1, f1) = compile r1 fresh progress in
     let (l2, f2) = compile r2 f1 progress in
     (l1 @ l2, f2)
  | Re_alt (r1, r2) ->
     let (l1, f1) = compile r1 (fresh+1) progress in
     let (l2, f2) = compile r2 (f1+1) progress in
     ([Fork (fresh+1, f1+1)] @ l1 @ [Jmp f2] @ l2, f2)
  | Re_quant (nul, qid, quant, r1) when progress = true ->
     begin match quant with
     | Star ->
        begin match nul with
        | NonNullable ->
           (* no need for BeginLoop/EndLoop instructions *)
           let (l1, f1) = compile r1 (fresh+2) true in
           ([Fork (fresh+1, f1+1); SetQuantToClock (qid,false)] @ l1 @ [Jmp fresh], f1+1)
        | CINullable | CDNullable ->
           (* nullable case: BeginLoop/EndLoop instructions needed for JS empty quantification semantics *)
          let (l1, f1) = compile r1 (fresh+3) true in
          ([Fork (fresh+1, f1+2); BeginLoop; SetQuantToClock (qid,false)] @ l1 @ [EndLoop; Jmp fresh], f1+2)
        end
     | LazyStar ->
        begin match nul with
        | NonNullable ->
           (* no need for BeginLoop/EndLoop instructions *)
           let (l1, f1) = compile r1 (fresh+2) true in
           ([Fork (f1+1, fresh+1); SetQuantToClock (qid,false)] @ l1 @ [Jmp fresh], f1+1)
        | CINullable | CDNullable ->
           (* nullable case: BeginLoop/EndLoop instructions needed for JS empty quantification semantics *)
           let (l1, f1) = compile r1 (fresh+3) true in
           ([Fork (f1+2, fresh+1); BeginLoop; SetQuantToClock (qid,false)] @ l1 @ [EndLoop; Jmp fresh], f1+2)
        end
     | Plus ->
        begin match nul with
        | NonNullable ->
           let (l1, f1) = compile r1 (fresh+1) true in
           ([SetQuantToClock (qid,false)] @ l1 @ [Fork (fresh,f1+1)], f1+1)
        | CINullable ->
           let (l1, f1) = compile r1 (fresh+3) true in
           ([Fork (fresh+1,f1+2); BeginLoop; SetQuantToClock (qid,false)] @ l1 @ [EndLoop; Fork (fresh+1,f1+3); SetQuantToClock (qid,true)], f1+3)
        | CDNullable ->
           (* Compiling as a concatenation in the nullable case. TODO: we can avoid duplication here *)
           let (l1, f1) = compile (Re_con(r1,Re_quant(nul,qid,Star,r1))) (fresh+1) true in
           ([SetQuantToClock (qid,false)] @ l1, f1)
        end
     | LazyPlus ->
        begin match nul with
        | NonNullable ->
           let (l1, f1) = compile r1 (fresh+1) true in
           ([SetQuantToClock (qid,false)] @ l1 @ [Fork (f1+1,fresh)], f1+1)
        | _ -> 
           (* Compiling as a concatenation in the nullable case *)
           let (l1, f1) = compile (Re_con(r1,Re_quant(nul,qid,LazyStar,r1))) (fresh+1) true in
           ([SetQuantToClock (qid,false)] @ l1, f1)
        end
     end
  (* when progress = false, ie we only want to find the top-priority nullable path *)
  | Re_quant (nul, qid, quant, r1) ->
     begin match quant with
     | Star | LazyStar ->
        (* you can just skip the stars, there's no way to null them by going inside since they don't allow empty repetitions *)
        ([],fresh)
     | Plus ->
        begin match nul with
        | NonNullable -> ([Fail], fresh+1) (* you won't be able to null that expression *)
        | CINullable ->                    (* only compile the null branch *)
           ([SetQuantToClock (qid,true)], fresh+1)
        | CDNullable ->
           let (l1, f1) = compile (Re_con(r1,Re_quant(nul,qid,Star,r1))) (fresh+1) false in
           ([SetQuantToClock (qid,true)] @ l1, f1)
             (* TODO: we want to compile the null branch only (no duplication), which requires a test *)
        end
     | LazyPlus ->
        begin match nul with
        | NonNullable -> ([Fail], fresh+1) (* you won't be able to null that expression *)            
        | _ ->                             (* duplication *)
           let (l1, f1) = compile (Re_con(r1,Re_quant(nul,qid,LazyStar,r1))) (fresh+1) false in
           ([SetQuantToClock (qid,true)] @ l1, f1)
             (* TODO: we can still eliminate the lazy star probably (it's going to get skipped by compilation anyway) *)
        end
     end
  | Re_capture (cid, r1) ->
     let (l1, f1) = compile r1 (fresh+1) progress in
     ([SetRegisterToCP (start_reg cid)] @ l1 @ [SetRegisterToCP (end_reg cid)], f1+1)
  | Re_lookaround (lookid, looktype, r1) ->
     (* does not compile the lookarounds it depends on, only the current expression *)
     begin match looktype with
     | Lookahead | Lookbehind -> ([CheckOracle lookid], fresh+1)
     | NegLookahead | NegLookbehind -> ([NegCheckOracle lookid], fresh+1)
     end

(* adds an accept at the end of the bytecode *)
let compile_to_bytecode (r:regex) : code =
  let (c,_) = compile r 0 true in
  let full_c = c @ [Accept] in
  Array.of_list full_c

(* same but with a WriteOracle instruction instead of an Accept *)
(* l is the current lookid we are compiling the regex for *)
let compile_to_write (r:regex) (l:lookid): code =
  let (c,_) = compile r 0 true in
  let full_c = c @ [WriteOracle l] in
  Array.of_list full_c

(* compile the bytecide for finding the nullable path *)
let compile_nullable (r:regex) : code =
  let (c,_) = compile r 0 false in
  let full_c = c @ [Accept] in
  Array.of_list full_c
