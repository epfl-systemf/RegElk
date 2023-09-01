(** * Bytecode Interpreter  *)
(* A VM-based interpreter of the bytecode that handles thread priority *)

open Bytecode
open Regex
open Map
open Array
open Format
open Oracle
open Compiler
open Cdn
open Anchors
open Regs
open Charclasses
       

(** * Direction  *)
(* In our algorithm, the interpreter can traverse the string in a forward way or in a backward way *)
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

(* just printing the contents of an Array for debugging purposes *)
let debug_regs (regs:int Array.t) : string =
      let s = ref "" in
      for c = 0 to (Array.length regs)-1 do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int (regs.(c)) ^ " | "
      done;
      !s
              
(* each thread stores capture registers for the capture groups it has matched so far *)
module Regs = (List_Regs : REGS)
              

(** * String Manipulation *)

let get_char (str:string) (cp:int) : char option =
  try Some (String.get str cp) with
  | Invalid_argument _ -> None    (* when reaching the end of the string *)
  
  
(** * Threads  *)

(* each thread has a program counter in the bytecode and a map of capture registers *)
type thread =
  {
    mutable pc: int;
    mutable capture_regs: Regs.regs; (* cp and clock for each capture group *)
    mutable look_regs: Regs.regs;    (* cp and clock for each lookaround *)
    mutable quant_regs: Regs.regs;   (* cp (if nulled) and clock for each quantifier *)
    mutable exit_allowed : bool;    (* are we allowed to exit the current loop *)
  }

let init_thread (initcap:Regs.regs) (initlook:Regs.regs) (initquant:Regs.regs): thread =
  { pc = 0; capture_regs = initcap; look_regs = initlook; quant_regs = initquant; exit_allowed = false }
  
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
let add_thread (t:thread) (x:char_expectation) (current:(thread*char_expectation) list) (inset:pcset) : (thread*char_expectation) list =
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
    mutable blocked: (thread*char_expectation) list;   (* threads stuck at a Consume instruction. low to high priority. *)
    mutable isblocked: pcset;       (* already blocked pcs, to avoid duplicates *)
    mutable bestmatch: thread option;   (* best match found so far, but there might be a higher priotity one still *)
    mutable context: char_context;             (* prev and next character at the current position *)
    mutable clock: int;                 (* global clock *)
    mutable cdn: cdn_table;             (* nullability table for cdn + *)
  }

(* initializing the context *)
(* when starting the interpreter at any position, possibly in the middle of the string *)
let cp_context (cp:int) (str:string) (dir:direction) : char_context =
  let nextop = get_char str cp in
  let prevop = get_char str (cp-1) in
  match dir with
  | Forward -> { prevchar = prevop ; nextchar = nextop }
  | Backward -> { prevchar = nextop ; nextchar = prevop }
  
let init_state (c:code) (initcp:int) (initcap:Regs.regs) (initlook:Regs.regs) (initquant:Regs.regs) (initclk:int) (initctx:char_context) =
  { cp = initcp;
    active = [init_thread initcap initlook initquant];
    processed = init_bpcset (size c);
    blocked = [];
    isblocked = init_pcset (size c);
    bestmatch = None;
    context = initctx;
    clock = initclk;
    cdn = init_cdn();
  }

(** * Debugging Utilities  *)

let print_exit_allowed (b:bool) : string =
  if b then "\027[32m✔\027[0m" else "\027[31m✘\027[0m"
  
let print_thread (t:thread) : string = string_of_int t.pc ^ print_exit_allowed t.exit_allowed

let print_active (l:thread list) : string =
  "  ACTIVE: " ^ List.fold_left (fun s t -> if (s = "") then (print_thread t) else (print_thread t) ^ ", " ^ s) "" l
  
let print_blk (b:thread * char_expectation) : string =
  "(" ^ print_thread (fst b) ^ ":" ^ expectation_to_string (snd b) ^ ")"
  
let print_blocked (l:(thread*char_expectation) list) : string =
  "  BLOCKED: " ^ List.fold_left (fun s b -> if (s = "") then (print_blk b) else (print_blk b) ^ ", " ^ s) "" l ^ "\n"

let print_cp (cp:int) : string =
  "  CP: " ^ string_of_int cp ^ "\n"

let print_match (b:thread option) =
  match b with
  | None -> "None\n"
  | Some t -> print_thread t ^ "\n" ^ Regs.to_string t.capture_regs

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
       s.clock <- s.clock + 1;  (* augmenting the global clock *)
       bpc_add s.processed t.pc t.exit_allowed; (* adding the current pc being handled to the set of proccessed pcs *)
       match i with
       | Consume ce -> (* adding the thread to the list of blocked thread if it isn't already there *)
          s.blocked <- add_thread t ce s.blocked s.isblocked; (* also updates isblocked *)
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
          s.active <- {pc = x;
                       capture_regs = Regs.copy t.capture_regs;
                       look_regs = Regs.copy t.look_regs;
                       quant_regs = Regs.copy t.quant_regs;
                       exit_allowed = t.exit_allowed}::s.active;
          advance_epsilon ~debug c s o
       | SetRegisterToCP r ->
          (* modifying the capture regs of the current thread *)
          t.capture_regs <- Regs.set_reg t.capture_regs r (Some s.cp) s.clock; 
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | SetQuantToClock (q,b) ->
          (* saving the current cp if we are nulling a + *)
          let ocp = if b then (Some s.cp) else None in
          (* adding the last iteration clock *)
          t.quant_regs <- Regs.set_reg t.quant_regs q ocp s.clock;
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | CheckOracle l ->
          if (get_oracle o s.cp l)
          then begin
              t.pc <- t.pc + 1; (* keeping the thread alive *)
              (* remembering the cp where we last needed the oracle *)
              t.look_regs <- Regs.set_reg t.look_regs l (Some s.cp) s.clock;
            end
          else s.active <- ac;  (* killing the thread *)
          advance_epsilon ~debug c s o
       | NegCheckOracle l ->
          if (get_oracle o s.cp l)
          then s.active <- ac   (* killing the thread *)
          else t.pc <- t.pc + 1;(* keeping the thread alive *)
          advance_epsilon ~debug c s o
       | WriteOracle l ->
          (* we reached a match but we want to write that into the oracle. we don't discard lower priority threads *)
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
       | CheckNullable qid ->
          if (cdn_get s.cdn qid)
          then t.pc <- t.pc+1   (* keeping the thread alive *)
          else s.active <- ac;  (* killing the thread *)
          advance_epsilon ~debug c s o
       | AnchorAssertion a ->
          if (is_satisfied a s.context)
          then t.pc <- t.pc+1   (* keeping the thread alive *)
          else s.active <- ac;  (* killing the thread *)
          advance_epsilon ~debug c s o
       | Fail ->
          s.active <- ac;       (* killing the current thread *)
          advance_epsilon ~debug c s o
     end

     
(* modifies the state by consuming the next character  *)
(* calls itself recursively until there are no more blocked threads *)
let rec consume ?(debug=false) (c:code) (s:interpreter_state): unit =
  match s.blocked with
  | [] -> ()
  | (t,ce)::blocked' ->
     s.blocked <- blocked';
     if (is_accepted s.context.nextchar ce) then
       begin t.exit_allowed <- true; t.pc <- t.pc + 1; s.active <- t::s.active end; (* adding t to the list of active threads *)
     (* since t just consumed something, we set its exit_allowed flag to true *)
     consume ~debug c s


(** * Null interpreter  *)
(* an interpreter that does not read the string, but instead simply follows epsilon transuitions *)
(* this is used to reconstruct the capture groups of last nulled plus iteration at the end of a match *)

let null_interp ?(debug=false) ?(verbose=false) (c:code) (s:interpreter_state) (o:oracle): thread option =
  if verbose then Printf.printf "%s CP%d\n" ("\n\027[36mNull Interpreter:\027[0m ") (s.cp);
  if verbose then Printf.printf "%s\n" (print_code c);
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
  s.bestmatch
  

(** * Interpreting the bytecode  *)
(* This functions assumes that s.context already contains the correct characters *)
let rec interpreter ?(debug=false) (c:code) (str:string) (s:interpreter_state) (o:oracle) (dir:direction) (cdn:cdns): thread option =
  if debug then
    begin
      Printf.printf "%s" (print_cp s.cp);
      Printf.printf "%s" (print_active s.active);
      Printf.printf "%s%!" (print_bestmatch s.bestmatch);
    end;

  (* building the CDN table *)
  s.cdn <- build_cdn cdn s.cp o s.context;
  if debug then
    begin
      Printf.printf "At CP%d, CDN table:%s\n" (s.cp) (print_cdn_table s.cdn (List.map fst cdn));
    end;
  
  (* follow epsilon transitions *)  
  advance_epsilon ~debug c s o;
  if debug then
    begin
      Printf.printf "%s\n%!" (print_blocked s.blocked);
    end;
  (* checking if there are still surviving threads *)
  match s.blocked, s.context.nextchar with
  | [], _ -> s.bestmatch        (* no more surviving threads *)
  | _, None -> s.bestmatch      (* we reached the end of the string *)
  | _, _ -> 
     (* advancing blocked threads *)
     consume ~debug c s;
     (* resetting the processed, blocked sets and the CDN table *)
     s.processed <- init_bpcset (size c); 
     s.isblocked <- init_pcset (size c);
     s.cdn <- init_cdn();
     (* advancing the current position *)
     s.cp <- incr_cp s.cp dir;
     (* updating the context *)
     let newchar = get_char str (s.cp - cp_offset dir) in
     update_context s.context newchar;
     (* recursive call *)
     interpreter ~debug c str s o dir cdn

          
(** * Reconstructing Nullable + Values  *)
(* when the winning thread of a match decided to go through the nullable path of a +, we might need to reconstruct any groups set during that nullable path *)

let reconstruct_plus_groups ?(debug=false) ?(verbose=false) (thread:thread) (r:regex) (s:string) (o:oracle) (dir:direction): thread =
  let capture = ref thread.capture_regs in
  let look = ref thread.look_regs in
  let quant = ref thread.quant_regs in
  (* goes through the regex, if it encounters a nulled +, it calls the null interpreter *)
  let rec nulled_plus (reg:regex) : unit =
    match reg with
    | Re_empty | Re_character _ -> ()
    | Re_alt (r1, r2) | Re_con (r1, r2) ->
       nulled_plus r1; nulled_plus r2
    | Re_capture (_,r1) -> nulled_plus r1
    | Re_lookaround (lid,lk,r1) -> () 
    | Re_anchor _ -> ()
    (* from shallowest to deepest plus: *)
    | Re_quant (nul,qid,quanttype,body) ->
       begin match (Regs.get_cp !quant qid) with
       | None -> nulled_plus body (* recursive call: the inner + may have been nulled *)
       | Some start_cp ->
          (* with no recursive calls: inner + will be reconstructed automatically *)
          let start_clock = int_of_opt (Regs.get_clock !quant qid) in
          let bytecode = compile_reconstruct_nulled body in
          let ctx = cp_context start_cp s dir in 
          let result = null_interp ~debug ~verbose bytecode (init_state bytecode start_cp !capture !look !quant start_clock ctx) o in
          begin match result with
          | None -> failwith "expected a nullable plus"
          | Some w ->             (* there's a winning thread when nulling *)
             (* updating all registers *)
             capture := w.capture_regs;
             look := w.look_regs;
             quant := w.quant_regs;
          end
       end
  in
  nulled_plus r;
  {pc = thread.pc; capture_regs = !capture; look_regs = !look; quant_regs = !quant; exit_allowed = thread.exit_allowed}
  
 
(** * Running the interpreter and returning its result  *)
  
(* running the interpreter on some code, with a particular initial interpreter state *)
(* also reconstructs the + groups *)
let interp ?(verbose = true) ?(debug=false) (r:regex) (c:code) (s:string) (o:oracle) (dir:direction) (start_cp:int) (capture:Regs.regs) (look:Regs.regs) (quant:Regs.regs) (start_clock:int) (cdn:cdns): thread option =
  if verbose then Printf.printf "%s - %s\n" ("\n\027[36mInterpreter:\027[0m "^s) (print_direction dir);
  if verbose then Printf.printf "%s\n" (print_code c);
  if verbose then Printf.printf "%s\n" (print_cdns cdn);
  if verbose then Printf.printf "%s\n" (print_context (cp_context start_cp s dir));
  let result = interpreter ~debug c s (init_state c start_cp capture look quant start_clock (cp_context start_cp s dir)) o dir cdn in
  (* reconstruct + groups *)
  let full_result = 
    match result with
    | None -> None
    | Some thread -> Some (reconstruct_plus_groups ~debug ~verbose thread r s o dir)
  in
  if verbose then Printf.printf "%s\n" ("\027[36mResult:\027[0m "^(print_match full_result));
  full_result


(* running the interpreter using the default initial state *)
let interp_default_init ?(verbose = true) ?(debug=false) (r:regex) (c:code) (s:string) (o:oracle) (dir:direction) (cdn:cdns): thread option =
  let maxcap = max_group r in
  let maxlook = max_lookaround r in
  let maxquant = max_quant r in
  let capture = Regs.init_regs (2*maxcap+2) in
  let look = Regs.init_regs (maxlook+1) in
  let quant = Regs.init_regs (maxquant+1) in
  interp ~verbose ~debug r c s o dir (init_cp dir (String.length s)) capture look quant 0 cdn

(* for tests, sometimes we only want to know if there is a match *)
let boolean_interp ?(verbose = true) ?(debug=false) (r:regex) (c:code) (s:string) (o:oracle) (dir:direction) (cdn:cdns): bool =
  match (interp_default_init ~verbose ~debug r c s o dir cdn) with
  | None -> false
  | _ -> true



(** * Filtering With Capture Reset  *)
(* At the end of the algorithm, we get many capture groups that should be reset *)
(* We use the quantifier registers to filter out those that are too old *)

(* modifies regs in-place *)
let rec filter_capture (r:regex) (cap_regs:int Array.t) (cap_clocks: int Array.t) (look_clocks:int Array.t) (quant_clocks:int Array.t) (maxclock:int) : unit =
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> ()
  | Re_alt (r1,r2) -> filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks maxclock;
                      filter_capture r2 cap_regs cap_clocks look_clocks quant_clocks maxclock
  | Re_con (r1,r2) -> filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks maxclock;
                      filter_capture r2 cap_regs cap_clocks look_clocks quant_clocks maxclock
  | Re_quant (nul, qid, quant, r1) ->
     let quant_val = quant_clocks.(qid) in (* the last time we went in *)
     if (quant_val < maxclock) then
       (* the last repetition of the inner quantifier happened before the last repetition of the outer one *)
       filter_all r1 cap_regs
     else
       filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks quant_val
  | Re_capture (cid, r1) ->
     let start = cap_clocks.(start_reg cid) in
     if (start < 0) then
       filter_all r1 cap_regs (* there is already no value for this capture group, we can clear everything inside *)
     else begin
         if (start < maxclock) (* cleaning the value of group cid (and everything inside) if its value is too old *)
         then begin cap_regs.(start_reg cid) <- -1; filter_all r1 cap_regs end
         else filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks maxclock
       end
  | Re_lookaround (lid, l, r1) ->
     let look_val = look_clocks.(lid) in (* the last time we needed the lookaround to hold *)
     if (look_val < 0) then
       filter_all r1 cap_regs (* we didn't need the lookaround: clear everything inside *)
     else begin
         if (look_val < maxclock) (* cleaning everything inside the lookaround since it's too old *)
         then filter_all r1 cap_regs
         else filter_capture r1 cap_regs cap_clocks look_clocks quant_clocks (-1)
                             (* resetting the maxclock to -1: lookaround clocks are reset *)
       end
      
and filter_all (r:regex) (regs:int Array.t) : unit = (* clearing all capture group inside a regex *)
  match r with
  | Re_empty | Re_character _ | Re_anchor _ -> ()
  | Re_alt (r1,r2) -> filter_all r1 regs; filter_all r2 regs
  | Re_con (r1,r2) -> filter_all r1 regs; filter_all r2 regs
  | Re_quant (nul, qid, quant, r1) ->
     filter_all r1 regs
  | Re_capture (cid, r1) ->
     regs.(start_reg cid) <- -1; filter_all r1 regs
  | Re_lookaround (lid, l, r1) -> filter_all r1 regs

(* we transform the registers to an Array with constant-time access and insertion when filtering *)
let filter_reset (r:regex) (capture:Regs.regs) (look:Regs.regs) (quant:Regs.regs) (maxclock:int) : int Array.t =
  let (cap_regs, cap_clocks) = Regs.to_arrays capture in
  let (_, look_clocks) = Regs.to_arrays look in
  let (_, quant_clocks) = Regs.to_arrays quant in
  filter_capture r cap_regs cap_clocks look_clocks quant_clocks maxclock;
  cap_regs
       

(** * Printing Results  *)
(* once the registers have been transformed to an array *)

let get_op (c:int Array.t) (reg:int) : int option =
  let value = c.(reg) in
  if (value < 0) then None else Some value

  
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
let print_cap_regs (c:int Array.t) (max_groups:int) (str:string) : string =
  let s = ref "" in
  for i = 0 to max_groups do
    s := !s ^ "#" ^ string_of_int i ^ ":";
    let startr = get_op c (start_reg i) in
    let endr = get_op c (end_reg i) in
    s := !s ^ print_slice str startr endr ^ "\n"
  done;
  !s

let print_cap_option (c:(int Array.t) option) (max_groups:int) (str:string) : string =
  match c with
  | None -> "NoMatch\n"
  | Some ca -> print_cap_regs ca max_groups str


let print_result ?(verbose=true) (r:regex) (str:string) (c:(int Array.t) option) : string =
  let s = ref "" in
  if verbose then
    s := !s ^ ("Result of matching " ^ print_regex r ^ " on string " ^ str ^ " : \n");
  let max = max_group r in
  !s ^ print_cap_option c max str ^ "\n"
          
