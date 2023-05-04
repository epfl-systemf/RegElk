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
  | Lookahead
  | NegLookahead
  | Lookbehind
  | NegLookbehind

(** * Raw Regexes  *)
(* Input regexes, before they get annotated with capture groups identifiers and lookaround identifiers *)
type raw_regex =
  | Raw_empty
  | Raw_char of char
  | Raw_alt of raw_regex * raw_regex
  | Raw_con of raw_regex * raw_regex
  | Raw_quant of quantifier * raw_regex
  | Raw_capture of raw_regex
  | Raw_lookaround of lookaround * raw_regex


(** * Annotated Regexes  *)

(* capture unique identifiers *)
type capture = int
(* lookaround unique identifiers *)
type lookid = int

type regex =
  | Re_empty
  | Re_char of char
  | Re_alt of regex * regex
  | Re_con of regex * regex
  (* in [quant cstart cend q r] we describe that capture groups in the interval [cstart, cend[ are defined inside the quantifier *)
  | Re_quant of capture * capture * quantifier * regex
  | Re_capture of capture * regex
  | Re_lookaround of lookid * lookaround * regex

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
  | Lookahead -> "?="
  | NegLookahead -> "?!"
  | Lookbehind -> "?<="
  | NegLookbehind -> "?<!"
                   
let rec print_raw (ra:raw_regex) : string =
  match ra with
  | Raw_empty -> ""
  | Raw_char ch -> String.make 1 ch
  | Raw_alt (r1, r2) -> print_raw r1 ^ "|" ^ print_raw r2
  | Raw_con (r1, r2) -> print_raw r1 ^ print_raw r2
  | Raw_quant (q, r1) -> print_raw r1 ^ print_quant q
  | Raw_capture r1 -> "(" ^ print_raw r1 ^ ")"
  | Raw_lookaround (l, r1) -> "(" ^ print_lookaround l ^ print_raw r1 ^ ")"

let rec print_regex (r:regex) : string =
  match r with
  | Re_empty -> ""
  | Re_char ch -> String.make 1 ch
  | Re_alt (r1, r2) -> print_regex r1 ^ "|" ^ print_regex r2
  | Re_con (r1, r2) -> print_regex r1 ^ print_regex r2
  | Re_quant (_, _, q, r1) -> print_regex r1 ^ print_quant q
  | Re_capture (cid, r1) -> "(" ^ print_regex r1 ^ ")" ^ "\027[33m" ^ string_of_int cid ^ "\027[0m"
  | Re_lookaround (lid, l, r1) -> "(" ^ "\027[36m" ^ string_of_int lid ^ "\027[0m" ^ print_lookaround l ^ print_regex r1 ^ ")"

                   
(** * Annotating Regexes  *)

(* Adds annotation, identifiers for each capture group and lookaround *)
(* Also returning the next fresh capture identifier and next fresh lookaround identifier *)
let rec annotate_regex (ra:raw_regex) (c:capture) (l:lookid) : regex * capture * lookid =
  match ra with
  | Raw_empty -> (Re_empty, c, l)
  | Raw_char ch -> (Re_char ch, c, l)
  | Raw_alt (r1, r2) ->
     let (ar1, c1, l1) = annotate_regex r1 c l in
     let (ar2, c2, l2) = annotate_regex r2 c1 l1 in
     (Re_alt (ar1, ar2), c2, l2)
  | Raw_con (r1, r2) ->
     let (ar1, c1, l1) = annotate_regex r1 c l in
     let (ar2, c2, l2) = annotate_regex r2 c1 l1 in
     (Re_con (ar1, ar2), c2, l2)
  | Raw_quant (quant, r1) ->
     let (ar1, c1, l1) = annotate_regex r1 c l in
     (Re_quant (c, c1, quant, ar1), c1, l1)
  | Raw_capture r1 ->
     let (ar1, c1, l1) = annotate_regex r1 (c+1) l in
     (Re_capture (c, ar1), c1, l1)
  | Raw_lookaround (look, r1) ->
     let (ar1, c1, l1) = annotate_regex r1 c (l+1) in
     (Re_lookaround (l, look, ar1), c1, l1)

(* adding annotations and adding a capture group on the entire regex *)
(* the external capture group starts at 0 *)
(* lookarounds start at 1 *)
let annotate (ra:raw_regex) : regex =
  let (re,_,_) = annotate_regex (Raw_capture ra) 0 1 in re
                                

(** * Regex Manipulation  *)

(* Reversing a regex when we want to execute it backward *)
(* Note that we only need to reverse the concatenation *)
(* For the alternation, the left subexpr still has priority over the right one when looking for groups *)
(* For instance, in [ab(?<=(ab|b))] over "ab", group 1 contains "ab", not "b" *)
let rec reverse_regex (r:regex) : regex =
  match r with
  | Re_empty | Re_char _ -> r
  | Re_alt (r1, r2) -> Re_alt (reverse_regex r1, reverse_regex r2)
  | Re_con (r1, r2) -> Re_con (reverse_regex r2, reverse_regex r1) (* reversing concatenation *)
  | Re_quant (cstart, cend, quant, r1) -> Re_quant (cstart, cend, quant, reverse_regex r1)
  | Re_capture (cid, r1) -> Re_capture (cid, reverse_regex r1)
  | Re_lookaround (lid, look, r1) -> Re_lookaround (lid, look, reverse_regex r1)


(* during the 1st stage of the algorithm, we don't care about extracting capture groups *)
(* so we can clean the regex to remove capture groups *)
(* We also remove the annotations in the quantifiers because there is no need to clear capture registers *)
let rec remove_capture (r:regex) : regex =
  match r with
  | Re_empty | Re_char _ -> r
  | Re_alt (r1, r2) -> Re_alt (remove_capture r1, remove_capture r2)
  | Re_con (r1, r2) -> Re_con (remove_capture r1, remove_capture r2)
  | Re_quant (cstart, cend, quant, r1) -> Re_quant (cstart, cstart, quant, remove_capture r1) (* cend = cstart: empty interval *)
  | Re_capture (cid, r1) -> remove_capture r1 (* removing the group entirely *)
  | Re_lookaround (lid, look, r1) -> Re_lookaround (lid, look, remove_capture r1)


(** * Lookaround Manipulation  *)

(* Extracting the lookaround type and inner subexpression of a given lookaround identifier *)
let rec get_lookaround (r:regex) (lid:lookid) : (regex * lookaround) option =
  match r with
  | Re_empty | Re_char _ -> None
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

(* Returns the maximum used lookaround in a regex *)
let rec max_lookaround (r:regex) : lookid =
  match r with
  | Re_empty | Re_char _ -> 0
  | Re_alt (r1, r2) | Re_con (r1, r2) -> max (max_lookaround r1) (max_lookaround r2)
  | Re_quant (_,_,_,r1) | Re_capture (_,r1) -> max_lookaround r1
  | Re_lookaround (lid, look, r1) -> max lid (max_lookaround r1)
