(** * Anchors  *)
(* here we describe how to match the different Anchors *)
(* Anchors need a two-character context: which characters are *)
(* just before and just after the current position *)

open Regex

(** * Char Contexts  *)
(* a context here describes the surrounding characters of a position (or None, if we reached the end/begin of the input) *)
(* this surrounding context is what the interpreter needs to evaluate anchors, and advance threads that reached a CONSUME instruction *)

type char_context =
  {
    mutable prevchar : char option;
    mutable nextchar : char option;
  }
(* NOTE: when going backward in the string, the index of nextchar is smaller than the one of prevchar *)

let update_context (ctx:char_context) (newchar:char option): unit =
  ctx.prevchar <- ctx.nextchar;
  ctx.nextchar <- newchar

let print_context (ctx:char_context) : string =
  "\027[36mContext:\027[0m " ^
    begin match ctx.prevchar,ctx.nextchar with
    | None,None -> "{None,None}"
    | None,Some x -> "{None,"^String.make 1 x^"}"
    | Some x,None -> "{"^String.make 1 x^",None}"
    | Some x, Some y -> "{"^String.make 1 x^","^String.make 1 y^"}"
    end ^ "\n"
    

(** * Checking Anchor Assertions  *)
(* https://tc39.es/ecma262/#ASCII-word-characters *)
let is_ascii_word_character (c:char) : bool =
  let n = int_of_char c in
  (n>=65 && n<=90) ||           (* uppercase *)
    (n>=97 && n<=122) ||        (* lowercase *)
      (n>=48 && n<=57) ||       (* numbers *)
        (n=95)                  (* '_' *)

let is_boundary (ctx:char_context) : bool =
  match ctx.prevchar, ctx.nextchar with
  | None, None -> false
  | None, Some _ -> true
  | Some _, None -> true
  | Some prev, Some next ->
     (is_ascii_word_character prev) <> (is_ascii_word_character next) (* xor *)
                                    
let is_satisfied (a:anchor) (ctx:char_context) : bool =
  match a with
  | BeginInput -> ctx.prevchar = None
  | EndInput -> ctx.nextchar = None
  | WordBoundary -> is_boundary ctx
  | NonWordBoundary -> not (is_boundary ctx)
