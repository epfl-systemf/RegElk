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

(* Compilation types *)
type comp_type =
  (* normal compilation type. making progress in the input string (Consume) is allowed *)
  | Progress
  (* Reconstructs the groups of a nulled regex. Recursively compile the nested +  *)
  | ReconstructNulled
  
(* Recursively compiles a regex *)
(* [fresh] is the next available instruction label *)
(* also returns the next fresh label after compilation *)
(* if the option progress is false, then this generates code that cannot make progress in the string *)
let rec compile (r:regex) (fresh:label) (ctype:comp_type) : instruction list * label =
  match r with
  | Re_empty -> ([], fresh)
  | Re_char ch ->
     begin match ctype with
     | Progress -> ([Consume ch], fresh+1)
     | ReconstructNulled -> ([Fail], fresh+1)
     end
  | Re_dot ->
     begin match ctype with
     | Progress -> ([ConsumeAll], fresh+1)
     | ReconstructNulled -> ([Fail], fresh+1)
     end
  | Re_con (r1, r2) ->
     let (l1, f1) = compile r1 fresh ctype in
     let (l2, f2) = compile r2 f1 ctype in
     (l1 @ l2, f2)
  | Re_alt (r1, r2) ->
     let (l1, f1) = compile r1 (fresh+1) ctype in
     let (l2, f2) = compile r2 (f1+1) ctype in
     ([Fork (fresh+1, f1+1)] @ l1 @ [Jmp f2] @ l2, f2)
  | Re_quant (nul, qid, quant, r1) when ctype = Progress ->
     (* progress compilation, consuming is allowed *)
     begin match quant with
     | Star ->
        begin match nul with
        | NonNullable ->
           (* no need for BeginLoop/EndLoop instructions *)
           let (l1, f1) = compile r1 (fresh+2) Progress in
           ([Fork (fresh+1, f1+1); SetQuantToClock (qid,false)] @ l1 @ [Jmp fresh], f1+1)
        | CINullable | CDNullable ->
           (* nullable case: BeginLoop/EndLoop instructions needed for JS empty quantification semantics *)
          let (l1, f1) = compile r1 (fresh+3) Progress in
          ([Fork (fresh+1, f1+2); BeginLoop; SetQuantToClock (qid,false)] @ l1 @ [EndLoop; Jmp fresh], f1+2)
        end
     | LazyStar ->
        begin match nul with
        | NonNullable ->
           (* no need for BeginLoop/EndLoop instructions *)
           let (l1, f1) = compile r1 (fresh+2) Progress in
           ([Fork (f1+1, fresh+1); SetQuantToClock (qid,false)] @ l1 @ [Jmp fresh], f1+1)
        | CINullable | CDNullable ->
           (* nullable case: BeginLoop/EndLoop instructions needed for JS empty quantification semantics *)
           let (l1, f1) = compile r1 (fresh+3) Progress in
           ([Fork (f1+2, fresh+1); BeginLoop; SetQuantToClock (qid,false)] @ l1 @ [EndLoop; Jmp fresh], f1+2)
        end
     | Plus ->
        begin match nul with
        | NonNullable ->
           let (l1, f1) = compile r1 (fresh+1) Progress in
           ([SetQuantToClock (qid,false)] @ l1 @ [Fork (fresh,f1+1)], f1+1)
        | CINullable ->
           let (l1, f1) = compile r1 (fresh+3) Progress in
           ([Fork (fresh+1,f1+2); BeginLoop; SetQuantToClock (qid,false)] @ l1 @ [EndLoop; Fork (fresh+1,f1+3); SetQuantToClock (qid,true)], f1+3)
        | CDNullable ->
           let (l1, f1) = compile r1 (fresh+3) Progress in
           ([Fork (fresh+1,f1+2); BeginLoop; SetQuantToClock (qid,false)] @ l1 @ [EndLoop; Fork (fresh+1,f1+4); CheckNullable qid; SetQuantToClock (qid,true)], f1+4)
        end
     | LazyPlus ->
        begin match nul with
        | NonNullable ->
           let (l1, f1) = compile r1 (fresh+1) Progress in
           ([SetQuantToClock (qid,false)] @ l1 @ [Fork (f1+1,fresh)], f1+1)
        | _ -> 
           (* Compiling as a concatenation in the nullable case *)
           let (l1, f1) = compile (Re_con(r1,Re_quant(nul,qid,LazyStar,r1))) (fresh+1) Progress in
           ([SetQuantToClock (qid,false)] @ l1, f1)
        end
     end
  (* when ctype = ReconstrutNulled, ie we only want to find the top-priority nullable path *)
  | Re_quant (nul, qid, quant, r1) ->
     begin match quant with
     | Star | LazyStar ->
        (* you can just skip the stars, there's no way to null them by going inside since they don't allow empty repetitions *)
        ([],fresh)
     | Plus ->
        begin match nul with
        | NonNullable -> ([Fail], fresh+1) (* you won't be able to null that expression *)
        | CINullable ->                    
           (* recursively compile the inner nested + *)
           let (l1, f1) = compile r1 (fresh+1) ReconstructNulled in
           ([SetQuantToClock (qid,true)] @ l1, f1)
        | CDNullable ->         
           (* recursively compile the inner nested + *)
           let (l1, f1) = compile r1 (fresh+1) ReconstructNulled in
           ([SetQuantToClock (qid,true)] @ l1, f1)
        end
     | LazyPlus ->
        begin match nul with
        | NonNullable -> ([Fail], fresh+1) (* you won't be able to null that expression *)            
        | _ ->
           let (l1, f1) = compile r1 (fresh+1) ctype in
           (* we don't need to reenter the star, compiling r1 once is enough *)
           ([SetQuantToClock (qid,true)] @ l1, f1)
        end
     end
  | Re_capture (cid, r1) ->
     let (l1, f1) = compile r1 (fresh+1) ctype in
     ([SetRegisterToCP (start_reg cid)] @ l1 @ [SetRegisterToCP (end_reg cid)], f1+1)
  | Re_lookaround (lookid, looktype, r1) ->
     (* does not compile the lookarounds it depends on, only the current expression *)
     begin match looktype with
     | Lookahead | Lookbehind -> ([CheckOracle lookid], fresh+1)
     | NegLookahead | NegLookbehind -> ([NegCheckOracle lookid], fresh+1)
     end

(* adds an accept at the end of the bytecode *)
let compile_to_bytecode (r:regex) : code =
  let (c,_) = compile r 0 Progress in
  let full_c = c @ [Accept] in
  Array.of_list full_c

(* same but with a WriteOracle instruction instead of an Accept *)
(* l is the current lookid we are compiling the regex for *)
let compile_to_write (r:regex) (l:lookid): code =
  let (c,_) = compile r 0 Progress in
  let full_c = c @ [WriteOracle l] in
  Array.of_list full_c

(* compiles the bytecode for reconstructing the missing groups from nulled + *)
(* this recursively compiles the nested + *)
let compile_reconstruct_nulled (r:regex) : code =
  let (c,_) = compile r 0 ReconstructNulled in
  let full_c = c @ [Accept] in
  Array.of_list full_c
  

