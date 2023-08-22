(** * Bytecode Compiler  *)
(* compile an (annotated) regex to bytecode *)

open Regex
open Bytecode


(** * Registers *)
   
(* defining registers corresponding to a given capture group *)
let start_reg (c:capture) : register = 2 * c
let end_reg (c:capture) : register = (2*c) + 1

(** * Compilation data-structure  *)
(* to avoid quadratic-time compilation by concatenating lists, we do the following: *)
(* we build a tree of lists of instructions during compilation *)
(* then we flatten this to a list of instructions *)
(* finally, we'll convert this to an arry of instructions that will be used by the interpreter *)
(* each of these steps should have linear time complexity in the regex *)

type 'x treelist =
  | Leaf of 'x list
  | Concat of ('x treelist) * ('x treelist)

let (@@) x y = Concat (x,y)

(* transforms a treelist into a list *)
(* linear time in the total number of 'x elements in t *)
let rec tl_flatten (t:'x treelist) (tail:'x list): 'x list =
  match t with
  | Leaf l -> l@tail            (* here we will iterate over the elements of l *)
  | Concat (t1,t2) ->
     let l2 = tl_flatten t2 tail in
     tl_flatten t1 l2
  
(** * Regex to Bytecode Compilation  *)

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
let rec compile (r:regex) (fresh:label) (ctype:comp_type) : instruction treelist * label =
  match r with
  | Re_empty -> (Leaf [], fresh)
  | Re_char ch ->
     begin match ctype with
     | Progress -> (Leaf [Consume ch], fresh+1)
     | ReconstructNulled -> (Leaf [Fail], fresh+1)
     end
  | Re_dot ->
     begin match ctype with
     | Progress -> (Leaf [ConsumeAll], fresh+1)
     | ReconstructNulled -> (Leaf [Fail], fresh+1)
     end
  | Re_con (r1, r2) ->
     let (l1, f1) = compile r1 fresh ctype in
     let (l2, f2) = compile r2 f1 ctype in
     (l1 @@ l2, f2)
  | Re_alt (r1, r2) ->
     let (l1, f1) = compile r1 (fresh+1) ctype in
     let (l2, f2) = compile r2 (f1+1) ctype in
     (Leaf [Fork (fresh+1, f1+1)] @@ l1 @@ Leaf [Jmp f2] @@ l2, f2)
     
  | Re_quant (nul, qid, quant, r1) when ctype = Progress ->
     (* progress compilation, consuming is allowed *)

     (* there is some code duplication here, but it should help identify the different particular cases that we do linearly *)

     (** particular case of the Non-Nullable +, where the last repetition can be used for the final loop *)
     if (quant.min > 0 && quant.max = None && nul = NonNullable) then
       (* repeat the body min-1 times *)
       let (min_code, min_fresh) = repeat_min (quant.min-1) qid r1 fresh ctype in
       (* loop on the last iteration *)
       let (body_code, body_fresh) = compile r1 (min_fresh+1) ctype in
       let fork = if quant.greedy then Fork (min_fresh, body_fresh+1)
                  else Fork (body_fresh+1, min_fresh) in
       (min_code @@ Leaf [SetQuantToClock (qid,false)] @@ body_code @@ Leaf [fork], body_fresh+1)
       
     (** particular case of the greedy CIN + *)
     else if (quant.min > 0 && quant.max = None && nul = CINullable && quant.greedy) then
       (* repeat the body min-1 times *)
       let (min_code, min_fresh) = repeat_min (quant.min-1) qid r1 fresh ctype in
       (* fork between the non-nullable repetition or the nulled branch *)
       let (body_code, body_fresh) = compile r1 (min_fresh+3) ctype in
       (min_code @@ Leaf [Fork (min_fresh+1, body_fresh+2); SetQuantToClock (qid, false); BeginLoop] @@ body_code @@ Leaf [EndLoop; Fork (min_fresh+1, body_fresh+3); SetQuantToClock (qid, true)],body_fresh+3)

     (** particular case of the greedy CDN + *)
     else if (quant.min > 0 && quant.max = None && nul = CDNullable && quant.greedy) then
       (* repeat the body min-1 times *)
       let (min_code, min_fresh) = repeat_min (quant.min-1) qid r1 fresh ctype in
       (* fork between the non-nullable repetition or the nulled branch with a CDN test *)
       let (body_code, body_fresh) = compile r1 (min_fresh+3) ctype in
       (min_code @@ Leaf [Fork (min_fresh+1, body_fresh+2); SetQuantToClock (qid, false); BeginLoop] @@ body_code @@ Leaf [EndLoop; Fork (min_fresh+1, body_fresh+4); CheckNullable qid; SetQuantToClock (qid, true)],body_fresh+4)

       
     (** Generic Case  *)
     else
       (* first repeat the body min times *)
       let (min_code, min_fresh) = repeat_min quant.min qid r1 fresh ctype in
       begin match quant.max with
       | None ->
          (* create a loop for the following repetitions *)
          (* iterations of this loop cannot match the empty string, hence the BeginLoop/EndLoop instructions *)
          (* TODO possible optimization: in the non-nullable case, I could remove BeginLoop/EndLoop *)
          let (iter_code, iter_fresh) = compile r1 (min_fresh+3) ctype in
          let fork = if quant.greedy then Fork(min_fresh+1, iter_fresh+2)
                     else Fork (iter_fresh+2,min_fresh+1) in
          (min_code @@ Leaf [fork; SetQuantToClock (qid, false); BeginLoop] @@ iter_code @@ Leaf [EndLoop; Jmp min_fresh],iter_fresh+2)
       | Some max ->
          (* repeat the optional repetitions max-min times *)
          let (opt_code, opt_fresh) = repeat_optional (max-quant.min) qid r1 min_fresh ctype quant.greedy in
          (min_code @@ opt_code,opt_fresh)
       end

  (* when ctype = ReconstrutNulled, ie we only want to find the top-priority nullable path *)
  | Re_quant (nul, qid, quant, r1) ->
     if (quant.min = 0) then (Leaf [], fresh) (* optional repetitions can't consume the empty string, so skip it *)
     else if (nul = NonNullable) then (Leaf [Fail], fresh+1) (* you won't be able to null that expression *)
     else
       (* in the case where min>0, and the body might be nullable  *)
       (* we only have to compile one iteration, since only the last iteration matters and iterations don't consume *)
       let (l1, f1) = compile r1 (fresh+1) ReconstructNulled in
       (Leaf [SetQuantToClock (qid,true)] @@ l1, f1)
         
  | Re_capture (cid, r1) ->
     let (l1, f1) = compile r1 (fresh+1) ctype in
     (Leaf [SetRegisterToCP (start_reg cid)] @@ l1 @@ Leaf [SetRegisterToCP (end_reg cid)], f1+1)
  | Re_lookaround (lookid, looktype, r1) ->
     (* does not compile the lookarounds it depends on, only the current expression *)
     begin match looktype with
     | Lookahead | Lookbehind -> (Leaf [CheckOracle lookid], fresh+1)
     | NegLookahead | NegLookbehind -> (Leaf [NegCheckOracle lookid], fresh+1)
     end
  | Re_anchor a -> (Leaf [AnchorAssertion a], fresh+1)

(* repeats a [r] regex [min] times when it's inside a quantifier [qid] with minimum repetitions *)
and repeat_min (min:int) (qid:quantid) (r:regex) (fresh:label) (ctype:comp_type) : instruction treelist * label =
  (* TODO possible optimization: there's no need to capture/set anything on the first min-1 repetitions *)
  if min = 0 then (Leaf [], fresh)
  else
    let (body_code, new_fresh) = compile r (fresh+1) ctype in
    let (next_code, next_fresh) = repeat_min (min-1) qid r new_fresh ctype in
    (Leaf [SetQuantToClock (qid,false)] @@ body_code @@ next_code, next_fresh)

(* repeats the optional max-min repetitions of a regex inside a bounded quantifier *)
(* those repetitions are not allowed to match the empty string, hence the BeginLoop/EndLoop instructions *)
and repeat_optional (nb:int) (qid:quantid) (r:regex) (fresh:label) (ctype:comp_type) (greedy:bool) : instruction treelist * label =
  if nb = 0 then (Leaf [], fresh)
  else
    let (body_code, new_fresh) = compile r (fresh+3) ctype in
    let (next_code, next_fresh) = repeat_optional (nb-1) qid r (new_fresh+1) ctype greedy in
    let fork = if greedy then Fork(fresh+1,next_fresh+1)
               else Fork(next_fresh+1,fresh+1) in
    (Leaf [fork; SetQuantToClock (qid,false); BeginLoop] @@ body_code @@ Leaf [EndLoop] @@ next_code,next_fresh+1)
  
    
                 
(* adds an accept at the end of the bytecode *)
let compile_to_bytecode (r:regex) : code =
  let (c,_) = compile r 0 Progress in
  let full_c = tl_flatten c [Accept] in
  Array.of_list full_c

(* same but with a WriteOracle instruction instead of an Accept *)
(* l is the current lookid we are compiling the regex for *)
let compile_to_write (r:regex) (l:lookid): code =
  let (c,_) = compile r 0 Progress in
  let full_c = tl_flatten c [WriteOracle l] in
  Array.of_list full_c

(* compiles the bytecode for reconstructing the missing groups from nulled + *)
(* this recursively compiles the nested + *)
let compile_reconstruct_nulled (r:regex) : code =
  let (c,_) = compile r 0 ReconstructNulled in
  let full_c = tl_flatten c [Accept] in
  Array.of_list full_c
  

