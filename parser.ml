(* parsing a string corresponding to a regex *)

open Regex
open Regex_parser
open Regex_lexer


type parse_result =
  | Unsupported
  | NotWF
  | OK of raw_regex

type support_stats = {
    mutable vtab:int;
    mutable named:int;
    mutable hex:int;
    mutable unicode:int;
    mutable prop:int;
    mutable backref:int;
    mutable notwf:int;
  }

let init_stats () : support_stats =
  { vtab=0; named=0; hex=0; unicode=0; prop=0; backref=0; notwf=0 }

let parse (str:string) (stats:support_stats): parse_result =
  try 
    let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
    if regex_wf r then OK r else begin stats.notwf <- stats.notwf + 1; NotWF end
  with 
  | Unsupported_Vtab -> stats.vtab <- stats.vtab + 1; Unsupported
  | Unsupported_named_groups -> stats.named <- stats.named + 1; Unsupported
  | Unsupported_hex -> stats.hex <- stats.hex + 1; Unsupported
  | Unsupported_unicode -> stats.unicode <- stats.unicode + 1; Unsupported
  | Unsupported_prop -> stats.prop <- stats.prop + 1; Unsupported
  | Unsupported_backref -> stats.backref <- stats.backref + 1; Unsupported


(* fails if the regex is not correct *)
let parse_raw (str:string) : raw_regex =
  let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
  assert (regex_wf r);
  r
                       
let print_stats (s:support_stats) : string =
  "Vtabs: " ^ string_of_int s.vtab ^
  "\nNamed Groups: " ^ string_of_int s.named ^
  "\nHex Escapes: " ^ string_of_int s.hex ^
  "\n Unicode Escapes: " ^ string_of_int s.unicode ^
  "\n Unicode Properties: " ^ string_of_int s.prop ^
  "\n Backreferences: " ^ string_of_int s.backref ^
  "\n Not WellFormed: " ^ string_of_int s.notwf ^ "\n"
    
   
