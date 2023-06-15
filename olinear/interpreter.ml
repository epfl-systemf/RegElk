(** * Bytecode Interpreter  *)
(* A VM-based interpreter of the bytecode that handles thread priority *)

open Bytecode
open Regex
open Map
open Array
open Format
open Oracle
open Compiler

(** * Direction  *)
(* In our algorithm, the interpreter can traverse the string in a forward way or ina backward way *)
(* Backward is used when building the oracle for lookaheads *)
(* Or when building capture groups for lookbehinds *)

type direction =
  | Forward
  | Backward

let print_direction (d:direction) : string =
  match d with
  | Forward -> "Forward"
  | Backward -> "Backward"

let oracle_direction (l:lookaround) : direction =
  match l with
  | Lookahead | NegLookahead -> Backward
  | Lookbehind | NegLookbehind -> Forward

let capture_direction (l:lookaround) : direction =
  match l with
  | Lookahead -> Forward
  | Lookbehind -> Backward
  | _ -> failwith "No capture on negative lookarounds"

(* increments the current position in the string depending on the direction *)
let incr_cp (cp:int) (dir:direction) : int =
  match dir with
  | Forward -> cp + 1
  | Backward -> cp - 1

let init_cp (dir:direction) (str_size:int) : int =
  match dir with
  | Forward -> 0
  | Backward -> str_size
(* there are (str_size+1) distinct positions: before and after the string count *)

(* when writing to the oracle, if we've been going backward we need an offset *)
let cp_offset (dir:direction) : int =
  match dir with
  | Forward -> 0
  | Backward -> 1


(** * Capture Registers  *)
(* TODO: why don't we switch to a int Array? We know how much capture regs we need *)
(* in that case we will need to make a copy of the Array for the forks, which is not constant-time *)
   
(* each thread stores capture registers for the capture groups it has matched so far *)
module IntMap = Map.Make(struct type t = int let compare = compare end)
              
type cap_regs = int IntMap.t

let set_reg (regs:cap_regs) (r:register) (v:int) : cap_regs =
  IntMap.add r v regs

let clear_reg (regs:cap_regs) (r:register) : cap_regs =
  IntMap.remove r regs
  
let get_reg (regs:cap_regs) (r:register) : int option =
  IntMap.find_opt r regs
              
let init_regs () : cap_regs =
  IntMap.empty

(** * Lookarounds Memory  *)
(* For the second stage of the algorithm, we need to remember when (which cp) we used the oracle *)
(* So that we can later get the capture groups defined in that lookaround *)

type look_mem = int IntMap.t

let set_mem (lm:look_mem) (lid:lookid) (cp:int) : look_mem =
  IntMap.add lid cp lm

let clear_mem (lm:look_mem) (lid:lookid) : look_mem =
  IntMap.remove lid lm
  
let get_mem (lm:look_mem) (lid:lookid) : int option =
  IntMap.find_opt lid lm

let init_mem (): look_mem =
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
    mutable mem: look_mem;
    mutable exit_allowed : bool;
  }

let init_thread (initregs:cap_regs) (initmem:look_mem) : thread =
  { pc = 0; regs = initregs; mem = initmem; exit_allowed = false }
  
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
let add_thread (t:thread) (x:char option) (current:(thread*(char option)) list) (inset:pcset) : (thread*(char option)) list =
  if (pc_mem inset t.pc) then current
  else begin
      pc_add inset t.pc;
      (t,x)::current
    end

(** * Boolean PC Sets  *)
  
(* For each PC of the bytecode, remembers if it has been handled for each possible state of the exit_allowed boolean *)
type bpcset =
  {
   true_set: pcset;
   false_set: pcset;
  }

let init_bpcset (bytecode_size: int) : bpcset =
  { true_set = init_pcset(bytecode_size); false_set = init_pcset(bytecode_size) }

let bpc_add (bpcs:bpcset) (pc:label) (exit_bool:bool) : unit =
  match exit_bool with
  | true -> pc_add bpcs.true_set pc
  | false -> pc_add bpcs.false_set pc

let bpc_mem (bpcs:bpcset) (pc:label) (exit_bool:bool) : bool =
  match exit_bool with
  | true -> pc_mem bpcs.true_set pc
  | false -> pc_mem bpcs.false_set pc

  
(** * Interpreter States  *)

(* The interpreter alternates between two different steps to keep threads synchronized:
   - first it advances all of its states through epsilon transitions, until they reach a Consume instruction
   - then it reads the next input character of the string at the current position and advances or kill each thread
 *)

type interpreter_state =
  {
    mutable cp: int;             (* current position in the input string *)
    mutable active: thread list;    (* ordered list of threads. high to low priority *)
    mutable processed: bpcset;       (* already processed pcs. Similar to isPcProcessed in Experimental  *)
    (* mutable because we reset it each step, but its modifications during a step are done in-place *)
    mutable blocked: (thread*(char option)) list;   (* threads stuck at a Consume instruction for a given char. low to high priority. *)
    mutable isblocked: pcset;       (* already blocked pcs, to avoid duplicates *)
    mutable bestmatch: thread option;   (* best match found so far, but there might be a higher priotity one still *)
    mutable nextchar: char;             (* next character to consume *)
  }

let init_state (c:code) (initcp:int) (initregs:cap_regs) (initmem:look_mem) =
  { cp = initcp;
    active = [init_thread initregs initmem];
    processed = init_bpcset (size c);
    blocked = [];
    isblocked = init_pcset (size c);
    bestmatch = None;
    nextchar = 'a';             (* this won't be used before it's set *)
  }

(** * Debugging Utilities  *)
  
let print_thread (t:thread) : string = string_of_int t.pc

let print_active (l:thread list) : string =
  "  ACTIVE: " ^ List.fold_left (fun s t -> if (s = "") then (print_thread t) else (print_thread t) ^ ", " ^ s) "" l

let print_charop (c:char option) : string =
  match c with
  | None -> "ALL"               (* when a consume expects any character (for the dot) *)
  | Some ch -> String.make 1 ch
  
let print_blk (b:thread * (char option)) : string =
  "(" ^ print_thread (fst b) ^ ":" ^ print_charop (snd b) ^ ")"
  
let print_blocked (l:(thread*(char option)) list) : string =
  "  BLOCKED: " ^ List.fold_left (fun s b -> if (s = "") then (print_blk b) else (print_blk b) ^ ", " ^ s) "" l ^ "\n"

let print_cp (cp:int) : string =
  "  CP: " ^ string_of_int cp ^ "\n"

let print_match (b:thread option) =
  match b with
  | None -> "None\n"
  | Some t -> print_thread t

let print_bestmatch (b:thread option) =
  "  BEST: " ^ print_match b ^ "\n"
  
(** * Interpreter  *)


  
(* modifies the state by advancing all threads along epsilon transitions *)
(* calls itself recursively until there are no more active threads *)
let rec advance_epsilon ?(debug=false) (c:code) (s:interpreter_state) (o:oracle) : unit =
  if debug then Printf.printf "%s\n%!" ("Epsilon active: " ^ print_active s.active);
    
  match s.active with
  | [] -> () (* done advancing epsilon transitions *)
  | t::ac -> (* t: highest priority active thread *)
     let i = get_instr c t.pc in
     if (bpc_mem s.processed t.pc t.exit_allowed) then (* killing the lower priority thread if it has already been processed *)
       begin s.active <- ac; advance_epsilon ~debug c s o end
     else begin
       bpc_add s.processed t.pc t.exit_allowed; (* adding the current pc being handled to the set of proccessed pcs *)
       match i with
       | Consume x -> (* adding the thread to the list of blocked thread if it isn't already there *)
          s.blocked <- add_thread t (Some x) s.blocked s.isblocked; (* also updates isblocked *)
          s.active <- ac;
          advance_epsilon ~debug c s o
       | ConsumeAll ->
          s.blocked <- add_thread t None s.blocked s.isblocked;
          s.active <- ac;
          advance_epsilon ~debug c s o
       | Accept ->             (* updates the best match and don't consider the remain active threads *)
          s.active <- [];
          s.bestmatch <- Some t;
          () (* no recursive call *)
       | Jmp x ->
          t.pc <- x;
          advance_epsilon ~debug c s o
       | Fork (x,y) ->           (* x has higher priority *)
          t.pc <- y;
          s.active <- {pc = x; regs = t.regs; mem = t.mem; exit_allowed = t.exit_allowed}::s.active;
          advance_epsilon ~debug c s o;
       | SetRegisterToCP r ->
          t.regs <- set_reg t.regs r s.cp; (* modifying the capture regs of the current thread *)
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | ClearRegister r ->
          t.regs <- clear_reg t.regs r;
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | ClearMemory l ->
          t.mem <- clear_mem t.mem l;
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | CheckOracle l ->
          if (get_oracle o s.cp l)
          then begin
              t.pc <- t.pc + 1; (* keeping the thread alive *)
              t.mem <- set_mem t.mem l s.cp (* remembering the cp where we last needed the oracle *)
            end
          else s.active <- ac;  (* killing the thread *)
          advance_epsilon ~debug c s o
       | NegCheckOracle l ->
          if (get_oracle o s.cp l)
          then s.active <- ac   (* killing the thread *)
          else t.pc <- t.pc + 1;(* keeping the thread alive *)
          advance_epsilon ~debug c s o
       | WriteOracle l ->
          (* we reached a match but we want to write that into the oracle. we don't discard lower priotity threads *)
          s.active <- ac;       (* no need to consider that thread anymore *)
          set_oracle o s.cp l;    (* writing to the oracle *)
          advance_epsilon ~debug c s o (* we keep searching for more matches *)
       | BeginLoop ->
       (* we need to set exit_allowed to false: now exiting a loop is forbidden according to JS semantics *)
          t.exit_allowed <- false;
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | EndLoop ->
          (* this transition is only possible if we didn't begin this loop during this epsilon transition phase *)
          begin match t.exit_allowed with
          | true -> t.pc <- t.pc+1; advance_epsilon ~debug c s o
          | false -> s.active <- ac; advance_epsilon ~debug c s o (* killing the current thread *)
          end
     end

let is_accepted (x:char) (o:char option): bool =
  match o with
  | None -> true                (* None means accept all *)
  | Some ch -> x = ch           (* when expecting a particular char *)
     
(* modifies the state by consuming the next character  *)
(* calls itself recursively until there are no more blocked threads *)
let rec consume ?(debug=false) (c:code) (s:interpreter_state): unit =
  match s.blocked with
  | [] -> ()
  | (t,x)::blocked' ->
     s.blocked <- blocked';
     if (is_accepted s.nextchar x) then
       begin t.exit_allowed <- true; t.pc <- t.pc + 1; s.active <- t::s.active end; (* adding t to the list of active threads *)
     (* since t just consumed something, we set its exit_allowed flag to true *)
     consume ~debug c s
     
  
let rec interpreter ?(debug=false) (c:code) (str:string) (s:interpreter_state) (o:oracle) (dir:direction) : thread option =
  if debug then
    begin
      Printf.printf "%s" (print_cp s.cp);
      Printf.printf "%s" (print_active s.active);
      Printf.printf "%s%!" (print_bestmatch s.bestmatch);
    end;
  (* follow epsilon transitions *)
  advance_epsilon ~debug c s o;
  if debug then
    begin
      Printf.printf "%s\n%!" (print_blocked s.blocked);
    end;
  (* read the next character *)
  let x = get_char str (s.cp - cp_offset dir)  in
  begin match x with
  | None -> s.bestmatch         (* we reached the end of the string *)
  | Some chr ->
     s.nextchar <- chr;
     (* advancing blocked threads *)
     consume ~debug c s;
     (* resetting the processed and blocked sets *)
     s.processed <- init_bpcset (size c); (* TODO: should we cache it to avoid recomputing? Or should that be constant time when we switch to arrays? *)
     s.isblocked <- init_pcset (size c);
     (* advancing the current position *)
     s.cp <- incr_cp s.cp dir;
     interpreter ~debug c str s o dir
  end
    (* TODO: we could detect when there are no more threads and don't go to the end of the string *)
    

let interp ?(verbose = true) ?(debug=false) (c:code) (s:string) (o:oracle) (dir:direction) (start_cp:int) (start_regs:cap_regs) (start_mem:look_mem): thread option =
  if verbose then Printf.printf "%s\n" ("\n\027[36mInterpreter:\027[0m "^s);
  if verbose then Printf.printf "%s\n" (print_code c);
  let result = interpreter ~debug c s (init_state c start_cp start_regs start_mem) o dir in
  if verbose then Printf.printf "%s\n" ("\027[36mResult:\027[0m "^(print_match result));
  result
  
let matcher_interp ?(verbose = true) ?(debug=false) (c:code) (s:string) (o:oracle) (dir:direction): thread option =
  if verbose then Printf.printf "%s\n" ("\n\027[36mInterpreter:\027[0m "^s);
  if verbose then Printf.printf "%s\n" (print_code c);
  let result = interpreter ~debug c s (init_state c (init_cp dir (String.length s)) (init_regs()) (init_mem()) ) o dir in
  if verbose then Printf.printf "%s\n" ("\027[36mResult:\027[0m "^(print_match result));
  result

(* for tests, sometimes we only want to know if there is a match *)
let match_interp ?(verbose = true) ?(debug=false) (c:code) (s:string) (o:oracle) (dir:direction): bool =
  match (matcher_interp ~verbose ~debug c s o dir) with
  | None -> false
  | _ -> true


(** * Printing Results  *)
(* extracting a capture group slice given its registers *)
let print_slice (str:string) (startreg:int option) (endreg:int option) : string =
  match startreg with
  | None -> "Undefined"
  | Some startv ->
     begin match endreg with
     | None -> failwith "startreg is set but not endreg"
     | Some endv ->
        if (endv >= startv) then
          String.sub str startv (endv - startv)
        else
          String.sub str endv (startv - endv)
                     (* capture groups defined in lookbehind have their capture regs reversed *)
     end

(* printing all capture groups *)
let print_cap_regs (c:cap_regs) (max_groups:int) (str:string) : string =
  let s = ref "" in
  for i = 0 to max_groups do
    s := !s ^ "#" ^ string_of_int i ^ ":";
    let startr = get_reg c (start_reg i) in
    let endr = get_reg c (end_reg i) in
    s := !s ^ print_slice str startr endr ^ "\n"
  done;
  !s

let print_cap_option (c:cap_regs option) (max_groups:int) (str:string) : string =
  match c with
  | None -> "NoMatch\n"
  | Some ca -> print_cap_regs ca max_groups str

let get_result (th:thread option) : cap_regs option =
  match th with
  | None -> None
  | Some t -> Some (t.regs)

let print_result ?(verbose=true) (r:regex) (str:string) (c:cap_regs option) : string =
  let s = ref "" in
  if verbose then
    s := !s ^ ("Result of matching " ^ print_regex r ^ " on string " ^ str ^ " : \n");
  let max = max_group r in
  !s ^ print_cap_option c max str ^ "\n"
          
