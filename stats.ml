(* Stats about the regex corpora *)


open Regex
open Regex_parser
open Regex_lexer
open Yojson
open Yojson.Basic.Util

(** * Gathering Statistics  *)
let rec has_groups (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_groups r1 || has_groups r2
  | Raw_quant (_,r1) | Raw_count (_,r1) | Raw_lookaround (_,r1) -> has_groups r1
  | Raw_capture _ -> true
   
(* detects regexes with a nullable quantifiers *)
(* that could be vulnerable to the semantic bug we fixed *)
let rec has_nullable (r:regex) : bool =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> false
  | Re_capture (_,r1) | Re_lookaround (_,_,r1) -> has_nullable r1
  | Re_con(r1,r2) | Re_alt (r1,r2) -> has_nullable r1 || has_nullable r2
  | Re_quant (nul,_,_,r1) ->
     has_nullable r1 || nul != NonNullable

let has_nullable_quant (raw:raw_regex) : bool =
  let r = annotate raw in
  has_nullable r

(* detects regexes with capture groups inside quantifiers *)
let rec groups_in_quant (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> groups_in_quant r1 || groups_in_quant r2
  | Raw_capture r1 | Raw_lookaround (_,r1) -> groups_in_quant r1
  | Raw_quant(_,r1) | Raw_count (_,r1) -> has_groups r1

(* detects regexes with lookarounds *)
let rec has_lookaround (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_lookaround r1 || has_lookaround r2
  | Raw_capture r1 | Raw_quant(_,r1) | Raw_count (_,r1) -> has_lookaround r1
  | Raw_lookaround _ -> true

(* detects regexes with quantifiers where the body is non-nullable and min>0 *)
(* these are regexes where our v8 fix without duplication is useful *)
let rec has_nn (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_nn r1 || has_nn r2
  | Raw_capture r1 | Raw_lookaround (_,r1) -> has_nn r1
  | Raw_quant (q,r1) ->
     begin match q with
     | Plus | LazyPlus -> has_nn r1 || raw_nullable r1 = NonNullable
     | _ -> has_nn r1
     end
  | Raw_count (q,r1) ->
     has_nn r1 || (raw_nullable r1 = NonNullable && q.min > 0)

(* detects regexes with CIN or CDN+ *)
(* regexes with a quantifier with nullable body and min>0 *)
(* but not the lazy ones, that we don't know how to handle linearly *)
let rec has_nullplus (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> has_nullplus r1 || has_nullplus r2
  | Raw_capture r1 | Raw_lookaround (_,r1) -> has_nullplus r1
  | Raw_quant (q,r1) ->
     begin match q with
     | Plus -> has_nullplus r1 || raw_nullable r1 = CINullable || raw_nullable r1 = CDNullable
     | _ -> has_nullplus r1
     end
  | Raw_count (q,r1) ->
     has_nullplus r1 || (raw_nullable r1 <> NonNullable && q.min > 0 && q.greedy)

(* detecting regexes that can be supported by the memoryless lookbehind only *)
(* they need to have lookbehinds without groups in them or negative lookbehinds *)
(* they also need to not have any lookaheads or positive lookbehinds with groups *)
exception NotMemoryLess

let rec lookbehind_only (r:raw_regex) : bool =
  match r with
  | Raw_empty | Raw_character _ | Raw_anchor _ -> false
  | Raw_con(r1,r2) | Raw_alt(r1,r2) -> lookbehind_only r1 || lookbehind_only r2
  | Raw_capture r1 | Raw_quant (_,r1) | Raw_count (_,r1) -> lookbehind_only r1
  | Raw_lookaround (l,r1) ->
     let _ = lookbehind_only r1 in (* possibly raising exceptions *)
     match l with
     | Lookahead | NegLookahead -> raise NotMemoryLess
     | NegLookbehind -> true
     | Lookbehind -> 
        if (has_groups r1) then raise NotMemoryLess else true

let memoryless_lookbehind (r:raw_regex) : bool =
  try (lookbehind_only r) with NotMemoryLess -> false

    
(** * Parsing and analysing the Corpus  *)
   
type parse_result =
  | Unsupported
  | NotWF
  | ParseError
  | OK of raw_regex

(* Statistics record when analyzing a vast corpus of regexes *)
type support_stats = {
    mutable named:int;
    mutable hex:int;
    mutable unicode:int;
    mutable prop:int;
    mutable backref:int;
    mutable octal:int;
    mutable notwf:int;
    mutable errors:int;
    mutable parsed:int;
    mutable total:int;
    mutable null_quant:int;
    mutable quant_groups:int;
    mutable lookaround:int;
    mutable nn:int;
    mutable null_plus:int;
    mutable ml_behind:int;
  }

let init_stats () : support_stats =
  { named=0; hex=0; unicode=0; prop=0; backref=0; notwf=0; octal=0;
    errors=0; parsed=0; total=0;
    null_quant=0; quant_groups=0; lookaround=0; nn=0; null_plus=0; ml_behind=0; }

(* parsing a string for a regex *)
let parse (str:string) (stats:support_stats): parse_result =
  try
    stats.total <- stats.total + 1;
    let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
    if regex_wf r
    then
      begin
        stats.parsed <- stats.parsed + 1;
        if (has_nullable_quant r) then stats.null_quant <- stats.null_quant + 1;
        if (groups_in_quant r) then stats.quant_groups <- stats.quant_groups + 1;
        if (has_lookaround r) then stats.lookaround <- stats.lookaround + 1;
        if (has_nn r) then stats.nn <- stats.nn + 1;
        if (has_nullplus r) then stats.null_plus <- stats.null_plus + 1;
        if (memoryless_lookbehind r) then stats.ml_behind <- stats.ml_behind + 1;
        OK r
      end
    else begin stats.notwf <- stats.notwf + 1; NotWF end
  with 
  | Unsupported_named_groups -> stats.named <- stats.named + 1; Unsupported
  | Unsupported_hex -> stats.hex <- stats.hex + 1; Unsupported
  | Unsupported_unicode -> stats.unicode <- stats.unicode + 1; Unsupported
  | Unsupported_prop -> stats.prop <- stats.prop + 1; Unsupported
  | Unsupported_backref -> stats.backref <- stats.backref + 1; Unsupported
  | Unsupported_octal -> stats.octal <- stats.octal + 1; Unsupported
  | _ -> stats.errors <- stats.errors + 1; ParseError


let print_result (p:parse_result): string =
  match p with
  | Unsupported -> "Unsupported"
  | NotWF -> "NotWF"
  | ParseError -> "ParseError"
  | OK r -> report_raw r

(* fails if the regex is not correct *)
let parse_raw (str:string) : raw_regex =
  let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
  assert (regex_wf r);
  r

(* printing statistics results *)
let print_stats (s:support_stats) : string =
  "Note that some octal escapes may be counted as backrefs here. Anyway, both are unsupported." ^
  "\nUnsupported Named Groups: " ^ string_of_int s.named ^
  "\nUnsupported Hex Escapes: " ^ string_of_int s.hex ^
  "\nUnsupported Unicode Escapes: " ^ string_of_int s.unicode ^
  "\nUnsupported Unicode Properties: " ^ string_of_int s.prop ^
  "\nUnsupported Backreferences: " ^ string_of_int s.backref ^
  "\nUnsupported Octal: " ^ string_of_int s.octal ^ 
  "\nNot WellFormed: " ^ string_of_int s.notwf ^
  "\nErrors: " ^ string_of_int s.errors ^
  "\nPARSED REGEXES / TOTAL REGEXES: " ^ string_of_int s.parsed ^ " / " ^ string_of_int s.total ^
  "\nNullable Quantifiers: " ^ string_of_int s.null_quant ^
  "\nCapture Groups in Quantifiers: " ^ string_of_int s.quant_groups ^
  "\nLookarounds: " ^ string_of_int s.lookaround ^
  "\nNonNullable, min>0 quantifiers: " ^ string_of_int s.nn ^
  "\nNullable Greedy min>0 quantifiers (CIN&CDN): " ^ string_of_int s.null_plus ^
  "\nMemoryLess Lookbehinds without groups: " ^ string_of_int s.ml_behind ^ 
  "\n"
    

let analyze_regex (regex_str:string) (stats:support_stats) =
  let result = parse regex_str stats in
  match result with
  | OK r -> if false then   (* select what you want to print *)
              begin Printf.printf "\n\027[36m%s\027[0m\n%!" regex_str; 
                    Printf.printf "%s\n%!" (print_result result)
              end
  | ParseError -> ()
  | _ -> ()
  
let analyze_corpus (filename:string) (single:bool) (st:support_stats option) : string =
  let stats =
    match st with
    | None -> init_stats()
    | Some s -> s in
  let chan = open_in filename in
  try
    while true; do
      let str = input_line chan in
      (* the list of all patterns defined on that line *)
      let regex_str = try
          let json_str = Yojson.Basic.from_string str in
          if single then 
            [json_str |> member "pattern" |> to_string]
          else
            List.map (to_string) (json_str |> member "patterns" |> to_list)
        with
        | Yojson.Json_error _ -> []
        | Yojson.Basic.Util.Type_error _ -> []
      in
      List.iter (fun str -> analyze_regex str stats) regex_str
    done; ""
  with End_of_file ->
    close_in chan;
    "\nCorpus \027[33m" ^ filename ^ "\027[0m:\n" ^ print_stats stats ^ "\n"

let analyze_single_corpus filename single: unit =
  let result = analyze_corpus filename single None in
  Printf.printf ("%s\n%!") result
  
  
let main =
  let corpora = [("corpus/npm-uniquePatterns.json",true);
                 ("corpus/pypi-uniquePatterns.json",true);
                 ("corpus/internetSources-regExLib.json",false);
                 ("corpus/internetSources-stackoverflow.json",false);
                 (* ("corpus/uniq-regexes-8.json",true) *)] in

  (* individual stats *)
  List.iter (fun (f,b) -> analyze_single_corpus f b) corpora;

  (* getting total stats *)
  let stats = init_stats() in
  List.iter (fun (f,b) -> ignore(analyze_corpus f b (Some stats))) corpora;
  Printf.printf ("\027[33mAll Corpus\027[0m:\n%s\n") (print_stats stats)
    
