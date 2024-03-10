(** * Regexes  *)
(* Defining the type of regexes that we match, including both capture groups and lookarounds *)

(** * Quantifiers  *)
(* By default, Star and Plus are greedy and try to consume as much of the string as possible *)
(* TODO: add QuestionMark? But how do we annotate it given that we don't need to clear registers? *)
(* We can annotate them anyway but decide during code generation not to clear any registers *)
type quantifier =
  | Star
  | LazyStar
  | Plus
  | LazyPlus

(** * Lookarounds  *)
type lookaround =
  | Lookbehind
  | NegLookbehind

(** * Raw Regexes  *)
(* Input regexes, before they get annotated with capture groups identifiers and lookaround identifiers *)
type raw_regex =
  | Raw_empty
  | Raw_char of char
  | Raw_dot
  | Raw_alt of raw_regex * raw_regex
  | Raw_con of raw_regex * raw_regex
  | Raw_quant of quantifier * raw_regex
  | Raw_capture of raw_regex
  | Raw_lookaround of lookaround * raw_regex


(** * Nullable Regexes  *)
(* Nullable quantifiers can be compiled differently *)
(* here we simply identify nullable regexes *)

(* There are 3 different types of nullability *)
(* Either a regex is non-nullable, meaning that all its paths lead to a Consume *)
(* Or it is Context-Independent Nullable, meaning that there is for sure a nullable path that does not depend on the string positiion *)
(* Or it is context-dependent nullable, for instance if its only nullable path goes through a lookaround or anchor *)

(* Note that sometimes we can classify CIN regexes as CDN (for instance if we go through a lookaround that is always true), *)
(* but our solution for CDN regexes also works for CIN (more general but slower) *)
                   
type nullability =
  | NonNullable                 (* it is impossible for the regex to be nulled *)
  | CDNullable                  (* teh regex can be nulled depending on the current string position *)
  | CINullable                  (* the regex can be nulled, and this does not depend on the current string position *)                        
                        
(** * Annotated Regexes  *)

(* capture unique identifiers *)
type capture = int
(* lookaround unique identifiers *)
type lookid = int
(* quantifiers unique identifiers *)
type quantid = int

type regex =
  | Re_empty
  | Re_char of char
  | Re_dot                      (* any character. TODO: should I generalize to ranges? *)
  | Re_alt of regex * regex
  | Re_con of regex * regex
  (* the first bool indicates if the body of the quantifier is nullable *)
  (* each quantifier is given an unique id *)
  | Re_quant of nullability * quantid * quantifier * regex
  | Re_capture of capture * regex
  | Re_lookaround of lookid * lookaround * regex


(** * Computing Nullability  *)
                   
let null_or (n1:nullability) (n2:nullability): nullability =
  match n1 with
  | NonNullable -> n2
  | CDNullable -> begin match n2 with
                  | CINullable -> CINullable
                  | _ -> CDNullable end
  | CINullable -> CINullable

let null_and (n1:nullability)  (n2:nullability): nullability =
  match n1 with
  | NonNullable -> NonNullable
  | CDNullable -> begin match n2 with
                  | NonNullable -> NonNullable
                  | _ -> CDNullable end
  | CINullable -> n2
  
let rec nullable (r:regex) : nullability =
  match r with
  | Re_empty -> CINullable
  | Re_char _ | Re_dot -> NonNullable
  | Re_alt (r1,r2) -> null_or (nullable r1) (nullable r2)
  | Re_con (r1,r2) -> null_and (nullable r1) (nullable r2)
  | Re_quant (_,_,q,r1) ->
     begin match q with
     | Star | LazyStar -> CINullable
     | Plus | LazyPlus -> nullable r1
     end
  | Re_capture (_,r1) -> nullable r1
  | Re_lookaround (_,_,_) -> CDNullable

                    
let rec raw_nullable (r:raw_regex) : nullability =
  match r with
  | Raw_empty -> CINullable
  | Raw_char _ | Raw_dot -> NonNullable
  | Raw_alt (r1,r2) -> null_or (raw_nullable r1) (raw_nullable r2)
  | Raw_con (r1,r2) -> null_and (raw_nullable r1) (raw_nullable r2)
  | Raw_quant (q,r1) ->
     begin match q with
     | Star | LazyStar -> CINullable
     | Plus | LazyPlus -> raw_nullable r1
     end
  | Raw_capture (r1) -> raw_nullable r1
  | Raw_lookaround (_,_) -> CDNullable

                   
(** * Regex Pretty-printing  *)

(* WARNING: this is wrong in the sense that we don't print non-capturing groups *)
(* Meaning that there will be no difference between [{ab} alt c] and [a alt {bc}] *)
(* If I want to add these non-capturing groups, I should find a way to only add those that are needed *)

let print_quant (q:quantifier) : string =
  match q with
  | Star -> "*"
  | LazyStar -> "*?"
  | Plus -> "+"
  | LazyPlus -> "+?"

let print_lookaround (l:lookaround) : string =
  match l with
  | Lookbehind -> "?<="
  | NegLookbehind -> "?<!"
                   
let rec print_raw (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_char ch -> String.make 1 ch
  | Raw_dot -> "."
  | Raw_alt (r1, r2) -> print_raw r1 ^ "|" ^ print_raw r2
  | Raw_con (r1, r2) -> print_raw r1 ^ print_raw r2
  | Raw_quant (q, r1) -> print_raw r1 ^ print_quant q
  | Raw_capture r1 -> "(" ^ print_raw r1 ^ ")"
  | Raw_lookaround (l, r1) -> "(" ^ print_lookaround l ^ print_raw r1 ^ ")"

let rec print_regex (r:regex) : string =
  match r with
  | Re_empty -> ""
  | Re_char ch -> String.make 1 ch
  | Re_dot -> "."
  | Re_alt (r1, r2) -> print_regex r1 ^ "|" ^ print_regex r2
  | Re_con (r1, r2) -> print_regex r1 ^ print_regex r2
  | Re_quant (_, qid, q, r1) -> print_regex r1 ^ print_quant q ^ "\027[31m" ^ string_of_int qid ^ "\027[0m"
  | Re_capture (cid, r1) -> "(" ^ print_regex r1 ^ ")" ^ "\027[33m" ^ string_of_int cid ^ "\027[0m"
  | Re_lookaround (lid, l, r1) -> "(" ^ "\027[36m" ^ string_of_int lid ^ "\027[0m" ^ print_lookaround l ^ print_regex r1 ^ ")"

                                
(** * Annotating Regexes  *)

(* Adds annotation, identifiers for each capture group and lookaround *)
(* Also returning the next fresh capture identifier and next fresh lookaround identifier *)
let rec annotate_regex (ra:raw_regex) (c:capture) (l:lookid) (q:quantid) : regex * capture * lookid  * quantid =
  match ra with
  | Raw_empty -> (Re_empty, c, l, q)
  | Raw_char ch -> (Re_char ch, c, l, q)
  | Raw_dot -> (Re_dot, c, l, q)
  | Raw_alt (r1, r2) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c l q in
     let (ar2, c2, l2, q2) = annotate_regex r2 c1 l1 q1 in
     (Re_alt (ar1, ar2), c2, l2, q2)
  | Raw_con (r1, r2) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c l q in
     let (ar2, c2, l2, q2) = annotate_regex r2 c1 l1 q1 in
     (Re_con (ar1, ar2), c2, l2, q2)
  | Raw_quant (quant, r1) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c l (q+1) in
     (Re_quant (nullable ar1, q, quant, ar1), c1, l1, q1)
  | Raw_capture r1 ->
     let (ar1, c1, l1, q1) = annotate_regex r1 (c+1) l q in
     (Re_capture (c, ar1), c1, l1, q1)
  | Raw_lookaround (look, r1) ->
     let (ar1, c1, l1, q1) = annotate_regex r1 c (l+1) q in
     (Re_lookaround (l, look, ar1), c1, l1, q1)

(* adding annotations and adding a capture group on the entire regex *)
(* the external capture group starts at 0 *)
(* lookarounds start at 1 *)
(* quants start at 1: we want to put the lazy star first *)
let annotate (ra:raw_regex) : regex =
  let (re,_,_,_) = annotate_regex (Raw_capture ra) 0 1 1 in re

(* Adds a .*? at the beginning of a regex so that it does not have to be matched at the beginning *)
let lazy_prefix (r:regex) : regex =
  Re_con (Re_quant (NonNullable, 0, LazyStar, Re_dot),r)

(** * Regex Manipulation  *)

(* Reversing a regex when we want to execute it backward *)
(* Note that we only need to reverse the concatenation *)
(* For the alternation, the left subexpr still has priority over the right one when looking for groups *)
(* For instance, in [ab(?<=(ab|b))] over "ab", group 1 contains "ab", not "b" *)
let rec reverse_regex (r:regex) : regex =
  match r with
  | Re_empty | Re_char _ | Re_dot -> r
  | Re_alt (r1, r2) -> Re_alt (reverse_regex r1, reverse_regex r2)
  | Re_con (r1, r2) -> Re_con (reverse_regex r2, reverse_regex r1) (* reversing concatenation *)
  | Re_quant (nul, qid, quant, r1) -> Re_quant (nul, qid, quant, reverse_regex r1)
  | Re_capture (cid, r1) -> Re_capture (cid, reverse_regex r1)
  | Re_lookaround (lid, look, r1) -> Re_lookaround (lid, look, reverse_regex r1)


(* during the 1st stage of the algorithm, we don't care about extracting capture groups *)
(* so we can clean the regex to remove capture groups *)
(* We also remove the annotations in the quantifiers because there is no need to clear capture registers *)
let rec remove_capture (r:regex) : regex =
  match r with
  | Re_empty | Re_char _ | Re_dot -> r
  | Re_alt (r1, r2) -> Re_alt (remove_capture r1, remove_capture r2)
  | Re_con (r1, r2) -> Re_con (remove_capture r1, remove_capture r2)
  | Re_quant (nul, qid, quant, r1) ->
     Re_quant (nul, qid, quant, remove_capture r1)
  (* TODO: we could have, instead of a quant identifier, an option *)
  (* when it's None, it means we don't have to remember the last iteration *)
  (* this would be useful in the first stage, or for the lazy star at the beginning *)
  | Re_capture (cid, r1) -> remove_capture r1 (* removing the group entirely *)
  | Re_lookaround (lid, look, r1) -> Re_lookaround (lid, look, remove_capture r1)


(** * Lookaround Manipulation  *)

(* Extracting the lookaround type and inner subexpression of a given lookaround identifier *)
let rec get_lookaround (r:regex) (lid:lookid) : (regex * lookaround) option =
  match r with
  | Re_empty | Re_char _ | Re_dot -> None
  | Re_alt (r1, r2) | Re_con (r1, r2) -> (* the order does not matter since each identifier is unique *)
     begin match (get_lookaround r1 lid) with
     | Some le -> Some le
     | None -> get_lookaround r2 lid
     end
  | Re_quant (_, _, _, r1) | Re_capture (_, r1)->
     get_lookaround r1 lid
  | Re_lookaround (l, look, r1) ->
     if (l = lid) then Some (r1, look)
     else get_lookaround r1 lid

(* we should always be able to find a lookaround in the right range *)
let get_look (r:regex) (lid:lookid) : regex * lookaround =
  match (get_lookaround r lid) with
  | Some r -> r
  | _ -> failwith "Cannot find lookaround"

(* extracting a quantifier body given its quantifier id *)
let rec get_quantifier (r:regex) (qid:quantid) : (regex * quantifier) option =
  match r with
  | Re_empty | Re_char _ | Re_dot -> None
  | Re_alt (r1, r2) | Re_con (r1, r2) ->
     begin match (get_quantifier r1 qid) with
     | Some qr -> Some qr
     | None -> get_quantifier r2 qid
     end
  | Re_lookaround (_, _, r1) | Re_capture (_, r1) ->
     get_quantifier r1 qid
  | Re_quant (nul,id,quant,r1) ->
     if (id = qid) then Some (r1, quant)
     else get_quantifier r1 qid

let get_quant (r:regex) (qid:quantid) : regex * quantifier =
  match (get_quantifier r qid) with
  | Some q -> q
  | _ -> failwith "Cannot find quantifier"

(* Returns the maximum used lookaround in a regex *)
let rec max_lookaround (r:regex) : lookid =
  match r with
  | Re_empty | Re_char _ | Re_dot -> 0
  | Re_alt (r1, r2) | Re_con (r1, r2) -> max (max_lookaround r1) (max_lookaround r2)
  | Re_quant (_, _,_,r1) | Re_capture (_,r1) -> max_lookaround r1
  | Re_lookaround (lid, look, r1) -> max lid (max_lookaround r1)

(* maximum capture group *)
let rec max_group (r:regex) : capture =
  match r with 
  | Re_empty | Re_char _ | Re_dot -> 0
  | Re_alt (r1, r2) | Re_con (r1, r2) -> max (max_group r1) (max_group r2)
  | Re_quant (_,_,_,r1) | Re_lookaround (_,_,r1) -> max_group r1
  | Re_capture (cid, r1) -> max cid (max_group r1)

(* Returns the list of nullable plus quantifier identifiers *)
(* ordered from lowest to highest *)
let rec nullable_plus_quantid' (r:regex) (lq:quantid list) : quantid list =
  match r with
  | Re_empty | Re_char _ | Re_dot -> lq
  | Re_alt (r1, r2) | Re_con (r1, r2) ->
     nullable_plus_quantid' r2 (nullable_plus_quantid' r1 lq)
  | Re_lookaround (_,_,r1) | Re_capture (_, r1) -> nullable_plus_quantid' r1 lq
  | Re_quant(nul,qid,quant,r1) ->
     begin match (quant,nul) with
     | (Plus,CDNullable) | (Plus,CINullable) ->
        nullable_plus_quantid' r1 (qid::lq)
     | _ -> nullable_plus_quantid' r1 lq
     end

let nullable_plus_quantid (r:regex) : quantid list =
  List.rev (nullable_plus_quantid' r [])

(* returns the list of all CDN plus *)
(* ordered from highest to lowest *)
let rec cdn_plus_list' (r:regex) (lq:quantid list) : quantid list =
  match r with
  | Re_empty | Re_char _ | Re_dot -> lq
  | Re_alt (r1, r2) | Re_con (r1, r2) ->
     cdn_plus_list' r2 (cdn_plus_list' r1 lq)
  | Re_lookaround (_,_,r1) | Re_capture (_, r1) -> cdn_plus_list' r1 lq
  | Re_quant (nul,qid,quant,r1) ->
     begin match (quant,nul) with
     | (Plus,CDNullable) -> cdn_plus_list' r1 (qid::lq)
     | _ -> cdn_plus_list' r1 lq
     end

let cdn_plus_list (r:regex) : quantid list =
  cdn_plus_list' r []
     

(** * Error Reporting  *)
(* we want to be able to print exactly the AST to the console so that we can copy paste it when the fuzzer finds a crash *)
let report_quant (q:quantifier) : string =
  match q with
  | Star -> "Star"
  | LazyStar -> "LazyStar"
  | Plus -> "Plus"
  | LazyPlus -> "LazyPlus"

let report_look (l:lookaround) : string =
  match l with
  | Lookbehind -> "Lookbehind"
  | NegLookbehind -> "NegLookbehind"
  
let rec report_raw (raw:raw_regex) : string =
  match raw with
  | Raw_empty -> "Raw_empty"
  | Raw_char x -> "Raw_char(\'"^String.make 1 x^"\')"
  | Raw_dot -> "Raw_dot"
  | Raw_alt (r1,r2) -> "Raw_alt("^report_raw r1^","^report_raw r2^")"
  | Raw_con (r1,r2) -> "Raw_con("^report_raw r1^","^report_raw r2^")"
  | Raw_quant (q,r1) -> "Raw_quant("^report_quant q^","^report_raw r1^")"
  | Raw_capture r1 -> "Raw_capture("^report_raw r1^")"
  | Raw_lookaround (l,r1) -> "Raw_lookaround("^report_look l^","^report_raw r1^")"


(** * Regex Plus Statistics  *)
(* returns (nn,cdn,cin,lnn,ln) where nn is the number of non-nullable +, *)
(* cdn is the number of context-dependent nullable + *)
(* cin is the number of context-independent nullable + *)
(* lnn is the number of non-nullable lazy + *)
(* ln is the number of nullable lazy + *)
let rec plus_stats (r:regex) : int * int * int * int * int =
  match r with
  | Re_empty | Re_char _ | Re_dot -> (0,0,0,0,0)
  | Re_alt (r1,r2) | Re_con (r1,r2) ->
     let (nn1,cdn1,cin1,lnn1,ln1) = plus_stats r1 in
     let (nn2,cdn2,cin2,lnn2,ln2) = plus_stats r2 in
     (nn1+nn2,cdn1+cdn2,cin1+cin2,lnn1+lnn2,ln1+ln2)
  | Re_lookaround (_,_,r1) | Re_capture (_,r1) ->
     plus_stats r1
  | Re_quant(nul,_,quant,r1) ->
     let (nn1,cdn1,cin1,lnn1,ln1) = plus_stats r1 in
     match quant with
     | Star | LazyStar -> (nn1,cdn1,cin1,lnn1,ln1)
     | Plus ->
        begin match nul with
        | NonNullable -> (nn1+1,cdn1,cin1,lnn1,ln1)
        | CDNullable -> (nn1,cdn1+1,cin1,lnn1,ln1)
        | CINullable -> (nn1,cdn1,cin1+1,lnn1,ln1)
        end
     | LazyPlus ->
        begin match nul with
        | NonNullable -> (nn1,cdn1,cin1,lnn1+1,ln1)
        | CDNullable | CINullable -> (nn1,cdn1,cin1,lnn1,ln1+1)
        end


(** * Checking Regex Validity  *)
(* In this version, we don't allow capture groups inside lookbehinds *)
       
let rec raw_regex_valid (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_char _ | Raw_dot -> true
  | Raw_alt(r1,r2) | Raw_con (r1,r2) -> raw_regex_valid r1 && raw_regex_valid r2
  | Raw_quant (q,r1) -> raw_regex_valid r1
  | Raw_capture r1 -> raw_regex_valid r1
  | Raw_lookaround (l,r1) -> no_capture r1
and no_capture (r:raw_regex) : bool = (* checks that there are no capture group *)
  match r with
  | Raw_empty | Raw_char _ | Raw_dot -> true
  | Raw_alt(r1,r2) | Raw_con (r1,r2) -> no_capture r1 && no_capture r2
  | Raw_quant (q,r1) -> no_capture r1
  | Raw_capture r1 -> false
  | Raw_lookaround (l,r1) -> no_capture r1
