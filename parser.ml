(* parsing a string corresponding to a regex *)

open Regex
open Regex_parser
open Regex_lexer


type parse_result =
  | Unsupported
  | NotWF
  | ParseError
  | OK of raw_regex

(* Statistics record when analyzing a vast corpus of regexes *)
type support_stats = {
    mutable vtab:int;
    mutable named:int;
    mutable hex:int;
    mutable unicode:int;
    mutable prop:int;
    mutable backref:int;
    mutable notwf:int;
    mutable errors:int;
    mutable total:int;
  }

let init_stats () : support_stats =
  { vtab=0; named=0; hex=0; unicode=0; prop=0; backref=0; notwf=0; errors=0; total=0 }

(* parsing a string for a regex *)
let parse (str:string) (stats:support_stats): parse_result =
  try
    stats.total <- stats.total + 1;
    let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
    if regex_wf r then OK r else begin stats.notwf <- stats.notwf + 1; NotWF end
  with 
  | Unsupported_Vtab -> stats.vtab <- stats.vtab + 1; Unsupported
  | Unsupported_named_groups -> stats.named <- stats.named + 1; Unsupported
  | Unsupported_hex -> stats.hex <- stats.hex + 1; Unsupported
  | Unsupported_unicode -> stats.unicode <- stats.unicode + 1; Unsupported
  | Unsupported_prop -> stats.prop <- stats.prop + 1; Unsupported
  | Unsupported_backref -> stats.backref <- stats.backref + 1; Unsupported
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
  "Unsupported Vtabs: " ^ string_of_int s.vtab ^
  "\nUnsupported Named Groups: " ^ string_of_int s.named ^
  "\nUnsupported Hex Escapes: " ^ string_of_int s.hex ^
  "\nUnsupported Unicode Escapes: " ^ string_of_int s.unicode ^
  "\nUnsupported Unicode Properties: " ^ string_of_int s.prop ^
  "\nUnsupported Backreferences: " ^ string_of_int s.backref ^
  "\nNot WellFormed: " ^ string_of_int s.notwf ^
  "\nErrors: " ^ string_of_int s.errors ^ 
  "\nTOTAL REGEXES: " ^string_of_int s.total ^ "\n"
    
   
