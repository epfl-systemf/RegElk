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

(** * String Manipulation *)

let get_char (str:string) (cp:int) : char option =
  try Some (String.get str cp) with
  | Invalid_argument _ -> None    (* when reaching the end of the string *)
  
  
(** * Threads  *)

(* each thread has a program counter in the bytecode and a map of capture registers *)
type thread =
  {
    mutable pc: int;
    mutable regs: cap_regs;
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
  
(* adds a thread and char at the head of a blocked list only if it's not already in *)
(* modifies the pcset in place *)
let add_thread (t:thread) (x:char) (current:(thread*char) list) (inset:pcset) : (thread*char) list =
  if (pc_mem inset t.pc) then current
  else begin
      pc_add inset t.pc;
      (t,x)::current
    end


(** * Interpreter States  *)

(* The interpreter alternates between two different steps to keep threads synchronized:
   - first it advances all of its states through epsilon transitions, until they reach a Consume instruction
   - then it reads the next input character of the string at the current position and advances or kill each thread
 *)

type interpreter_state =
  {
    mutable cp: int;             (* current position in the input string *)
    mutable active: thread list;    (* ordered list of threads. high to low priority *)
    mutable processed: pcset;       (* already processed pcs. Similar to isPcProcessed in Experimental  *)
    (* mutable because we reset it each step, but its modifications during a step are done in-place *)
    mutable blocked: (thread*char) list;   (* threads stuck at a Consume instruction for a given char. low to high priority. *)
    mutable isblocked: pcset;       (* already blocked pcs, to avoid duplicates *)
    mutable bestmatch: thread option;   (* best match found so far, but there might be a higher priotity one still *)
    mutable nextchar: char;             (* next character to consume *)
  }

(** * Interpreter  *)
  
(* modifies the state by advancing all threads along epsilon transitions *)
(* calls itself recursively until there are no more active threads *)
let rec advance_epsilon (c:code) (s:interpreter_state) : unit =
  match s.active with
  | [] -> () (* done advancing epsilon transitions *)
  | t::ac -> (* t: highest priority active thread *)
     let i = get_instr c t.pc in
     if (pc_mem s.processed t.pc) then (* killing the lower priority thread if it has already been processed *)
       begin s.active <- ac; advance_epsilon c s end
     else
       begin match i with
       | Consume x -> (* adding the thread to the list of blocked thread if it isn't already there *)
          s.blocked <- add_thread t x s.blocked s.isblocked; (* also updates isblocked *)
          s.active <- ac;
          advance_epsilon c s
       | Accept ->              (* updates the best match and don't consider the remain active threads *)
          (* TODO: depends on the stage *)
          s.active <- [];
          s.bestmatch <- Some t;
          () (* no recursive call *)
       | Jmp x ->
          t.pc <- x;
          advance_epsilon c s
       | Fork (x,y) ->            (* x has higher priority *)
          t.pc <- y;
          s.active <- {pc = x; regs = t.regs}::s.active;
          advance_epsilon c s
       | SetRegisterToCP r ->
          t.regs <- set_reg t.regs r s.cp; (* modifying the capture regs of the current thread *)
          t.pc <- t.pc + 1;
          advance_epsilon c s
       | ClearRegister r ->
          t.regs <- clear_reg t.regs r;
          t.pc <- t.pc + 1;
          advance_epsilon c s
       | CheckOracle l -> failwith "TODO oracle"
       | NegCheckOracle l -> failwith "TODO oracle"
       end

(* modifies the state by consuming the next character  *)
(* calls itself recursively until there are no more blocked threads *)
let rec consume (c:code) (s:interpreter_state): unit =
  match s.blocked with
  | [] -> ()
  | (t,x)::blocked' ->
     s.blocked <- blocked';
     if (x = s.nextchar) then
       begin t.pc <- t.pc + 1; s.active <- t::s.active end; (* adding t to the list of active threads *)
     consume c s
     
  
let rec interpreter (c:code) (str:string) (s:interpreter_state) : thread option =
  (* follow epsilon transitions *)
  advance_epsilon c s;
  (* read the next character *)
  let x = get_char str s.cp in
  begin match x with
  | None -> s.bestmatch         (* we reached the end of the string *)
  | Some chr ->
     s.nextchar <- chr;
     (* advancing blocked threads *)
     consume c s;
     (* resetting the processed and blocked sets *)
     s.processed <- init_pcset (size c); (* TODO: should we cache it to avoid recomputing? Or should that be constant time when we switch to arrays? *)
     s.isblocked <- init_pcset (size c);
     (* advancing the current position *)
     s.cp <- s.cp + 1;
     interpreter c str s
  end
    (* TODO: we could detect when there are no more threads and don't go to the end of the string *)
    
