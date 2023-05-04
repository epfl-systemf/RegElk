(** * Bytecode Interpreter  *)
(* A VM-based interpreter of the bytecode that handles thread priority *)

open Bytecode
open Regex
open Map
open Array

(** * Capture Registers  *)
(* each thread stores capture registers for the capture groups it has matched so far *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
              
type cap_regs = int IntMap.t

let set_reg (regs:cap_regs) (r:register) (v:int) : cap_regs =
  IntMap.add r v regs

let clear_reg (regs:cap_regs) (r:register) : cap_regs =
  IntMap.remove r regs
  
let get_reg (regs:cap_regs) (r:register) : int =
  IntMap.find r regs
              
let init_regs : cap_regs =
  IntMap.empty
  
(** * Threads  *)

(* each thread has a program counter in the bytecode and a map of capture registers *)
type thread =
  {
    pc: int;
    regs: cap_regs;
  }

(** * PC Sets  *)

(* for each PC of the bytecode, we want to track if, in a step of the interpreter, this PC has already been processed *)
(* this prevents us from handling the same pc twice: only the top priority thread with that pc should be handled *)
(* We use a mutable array of booleans, since we know the size of the bytecode *)
type pcset = bool Array.t

let init_pcset (bytecode_size: int) : pcset =
  assert (bytecode_size > 0);
  Array.make bytecode_size false

(* add a pc to the set after it has been processed *)
let pc_add (pcs:pcset) (pc:label) : unit =
  pcs.(pc) <- true

(* check if a pc has been processed already *)
let pc_mem (pcs:pcset) (pc:label) : bool =
  pcs.(pc)
  
(* adds a thread at the head of a list only if it's not already in *)
(* modifies the pcset in place *)
let add_thread (t:thread) (current:thread list) (inset:pcset) : thread list =
  if (pc_mem inset t.pc) then current
  else begin
      pc_add inset t.pc;
      t::current
    end


(** * Interpreter States  *)

(* The interpreter alternates between two different steps to keep threads synchronized:
   - first it advances all of its states through epsilon transitions, until they reach a Consume instruction
   - then it reads the next input character of the string at the current position and advances or kill each thread
 *)

type epsilon_state =            (* while following epsilon transitions *)
  {
    cp: int;                (* current position in the input string *)
    str: string;            (* input string *)
    active: thread list;    (* ordered list of threads. high to low priority *)
    processed: pcset;       (* already processed pcs. Similar to isPcProcessed in Experimental  *)
    blocked: thread list;   (* threads stuck at a Consume instruction: finished following epsilon transitions. low to high.  *)
    isblocked: pcset;       (* already blocked pcs, to avoid duplicates *)
    bestmatch: thread option;   (* best match found so far, but there might be a higher priotity one still *)
  }

type done_epsilon_state =            (* the result of following espilon transitions *)
  {
    cp: int;
    str: string;
    blocked: thread list;
    bestmatch: thread option;
  }

type consuming_state =          (* while consuming a character *)
  {
    cp: int;
    str: string;
    x: char;                    (* current character to consume *)
    blocked: thread list;
    next: thread list;          (* list of alive threads after consuming. high to low priority *)
    bestmatch: thread option;
  }

type done_consuming_state =     (* the result of consuming *)
  {
    cp: int;
    str: string;
    next: thread list;
    bestmatch: thread option;
  }
  
  
type interpreter_state =
  | Epsilon of epsilon_state
  | Done_Epsilon of done_epsilon_state
  | Consuming of consuming_state
  | Done_Consuming of done_consuming_state
  | EOE of thread option        (* end of execution *)
