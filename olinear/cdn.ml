(** * CDN Nullability Check  *)
(* for context-dependent nullable eager plus, the nullability of the body depends on the current position *)
(* to be able to match them without bytecode duplication, we need to know when is each + nullable *)
(* CDN formulas are a way to express when is a regex nullable *)

(* the goal is to compile each CDN plus to a CDN formula *)
(* and use these formulas to compute the CDN table at each string position *)
(* This CDN table is then used by the interpreter *)

open Regex
open Oracle
open Bytecode

(** * CDN Table  *)
(* For all Context-Dependent Nullable Plusses, we need to remember *)
(* when each of them is nullable at a given cp *)
   
module IntMap = Map.Make(struct type t = int let compare = compare end)        
type cdn_table = unit IntMap.t
(* when a unit is set for a given id, it means the corresponding quantifier is nullable *)
               
let init_cdn () : cdn_table =
  IntMap.empty

let cdn_set_true (cdn:cdn_table) (qid:quantid) : cdn_table =
  IntMap.add qid () cdn

let cdn_get (cdn:cdn_table) (qid:quantid) : bool =
  match (IntMap.find_opt qid cdn) with
  | Some _ -> true
  | None -> false
          
(** * CDN formulas  *)
(* the nullability of a regex may depend on the nullability of another quantifier *)
(* or on whether or not a lookaround holds *)
type cdn_formula =
  | CDN_true
  | CDN_false
  | CDN_and of cdn_formula * cdn_formula
  | CDN_or of cdn_formula * cdn_formula
  | CDN_quant of quantid
  | CDN_look of lookid
  | CDN_neglook of lookid


(** * Evaluating CDN formulas  *)
let rec interpret_cdn (f:cdn_formula) (cp:int) (o:oracle) (t:cdn_table) : bool =
  match f with
  | CDN_true -> true
  | CDN_false -> false
  | CDN_and (f1, f2) -> (interpret_cdn f1 cp o t) && (interpret_cdn f2 cp o t)
  | CDN_or (f1, f2) -> (interpret_cdn f1 cp o t) || (interpret_cdn f2 cp o t)
  | CDN_quant qid -> cdn_get t qid
  | CDN_look lid -> get_oracle o cp lid
  | CDN_neglook lid -> not (get_oracle o cp lid)
  
                                
(** * Compiling to CDN formulas *)                            
(* generates the formula that expresses when a regex is nullable *)
(* this minimizes the formula as we are building it *)
let rec compile_cdnf (r:regex) : cdn_formula =
  match r with
  | Re_empty -> CDN_true
  | Re_char _ | Re_dot -> CDN_false
  | Re_alt (r1, r2) ->
     let f1 = compile_cdnf r1 in
     let f2 = compile_cdnf r2 in
     begin match f1,f2 with
     | CDN_true, _ -> CDN_true
     | _, CDN_true -> CDN_true
     | CDN_false, _ -> f2
     | _, CDN_false -> f1
     | _, _ -> CDN_or (f1, f2)
     end
  | Re_con (r1, r2) ->
     let f1 = compile_cdnf r1 in
     let f2 = compile_cdnf r2 in
     begin match f1,f2 with
     | CDN_true, _ -> f2
     | _, CDN_true -> f1
     | CDN_false, _ -> CDN_false
     | _, CDN_false -> CDN_false
     | _, _ -> CDN_and (f1, f2)
     end
  | Re_quant (nul, qid, quant, r1) ->
     begin match quant with
     | Star | LazyStar -> CDN_true
     | Plus ->
        begin match nul with
        | NonNullable -> CDN_false
        | CINullable -> CDN_true
        | CDNullable -> CDN_quant qid
        end
     | LazyPlus ->
        begin match nul with
        | NonNullable -> CDN_false
        | CINullable -> CDN_true
        | CDNullable -> compile_cdnf r1
        end
     end
  | Re_capture (cid, r1) -> compile_cdnf r1
  | Re_lookaround (lid, look, r1) ->
     begin match look with
     | Lookahead | Lookbehind -> CDN_look lid
     | NegLookahead | NegLookbehind -> CDN_neglook lid
     end

(** TODO: adapt what's below  *)
    
(** * CDN codes  *)
(* here we define the set of bytecode used by the interpreter to update, at each *)
(* string position, which CDN plus is nullable *)

(* associates to each cdn quantifier id its nullable code *)
type cdn_codes = code IntMap.t
                      
let cdn_code_init () : cdn_codes =
  IntMap.empty

let get_cdn_code (codes:cdn_codes) (qid:quantid) : code =
  match (IntMap.find_opt qid codes) with
  | Some c -> c
  | None -> failwith "couldn't find this CDN code"

(* both the codes and the list of CDN identifiers from highest to lowest *)
type cdns = cdn_codes * quantid list

(** * Compiling CDN codes  *)
(* for each regex, we also compile the cdn codes of each cdn plus *)
(* these codes will be run at each step of the interpreter *)
(* to build up the CDN table *)
let compile_cdn_codes (r:regex) : cdns =
  (* let codes = ref (cdn_code_init()) in
   * let cdn_list = cdn_plus_list r in
   * List.iter (fun qid ->
   *     let (body,_) = get_quant r qid in
   *     let bytecode = compile_test_nullable body in
   *     codes := IntMap.add qid bytecode !codes
   *   ) cdn_list;
   * (!codes, cdn_list) *)
  failwith "TODO"
     
(** * Building the CDN Table  *)
(* TODO *)
let rec build_cdn () (cp:int) (o:oracle) : cdn_table =
  let table = ref (init_cdn()) in
  !table
   (* TODO *)
