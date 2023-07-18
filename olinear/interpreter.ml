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

(* Saving the last clocks of each capture groups *)
type cap_clocks = cap_regs

(** * Quantifier Clocks  *)
(* Remembering what's the global clock of the last time we went into each quantifier *)
(* the option is [Some x], when that quantifier is a +, and it's last iteration consisted in *)
(* nulling the + at cp [x] *)
(* Otherwise it is [None], for stars or for non-nulled iterations of + *)
type quant_clocks = (int*(int option)) IntMap.t

let set_quant (qc:quant_clocks) (q:quantid) (cp:int) (o:int option) : quant_clocks =
  IntMap.add q (cp,o) qc

(* right now this returns -1 by default. We might change to an option *)
let get_quant_clock (qc:quant_clocks) (q:quantid) : int  =
  match (IntMap.find_opt q qc) with
  | None -> -1
  | Some x -> fst x

let get_quant_nulled (qc:quant_clocks) (q:quantid) : int option =
  match (IntMap.find_opt q qc) with
  | None -> None
  | Some x -> snd x
            
let init_quant_clocks () : quant_clocks =
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

(* saving the last clock of each lookaround *)
type look_clocks = look_mem

(** * String Manipulation *)

let get_char (str:string) (cp:int) : char option =
  try Some (String.get str cp) with
  | Invalid_argument _ -> None    (* when reaching the end of the string *)
  
  
(** * Threads  *)

(* each thread has a program counter in the bytecode and a map of capture registers *)
type thread =
  {
    mutable pc: int;
    mutable regs: cap_regs; (* the value of capture groups - string indices *)
    mutable cap_clk : cap_clocks; (* the clocks of capture groups *)
    mutable mem: look_mem;        (* the string indices at which we last used each lookaound *)
    mutable look_clk : look_clocks; (* the clock at which we last used each lookaround *)
    mutable quants: quant_clocks;   (* the clock (and nulled cp) at which we last entered each quantifier *)
    mutable exit_allowed : bool;    (* are we allowed to exit the current loop *)
  }

let init_thread (initregs:cap_regs) (initcclock:cap_clocks) (initmem:look_mem) (initlclock:look_clocks) (initquants:quant_clocks): thread =
  { pc = 0; regs = initregs; cap_clk = initcclock; mem = initmem; look_clk = initlclock; quants = initquants; exit_allowed = false }
  
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

(** * CDN Table  *)
(* For all Context-Dependent Nullable Plusses, we need to remember *)
(* when each of them is nullable at a given cp *)
           
type cdn_table = unit IntMap.t
(* when a unit is set for a given id, it means the corresponding quantifier is nullable *)
               
let init_cdn () : cdn_table =
  IntMap.empty

let cdn_set_true (cdn:cdn_table) (qid:quantid) : cdn_table =
  IntMap.add qid () cdn

let cdn_get (cdn:cdn_table) (qid:quantid) : bool =
  match (IntMap.find_opt qid cdn) with
  | Some _ -> true
  | None -> false
           
  
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
    mutable clock: int;                 (* global clock *)
    mutable cdn: cdn_table;             (* nullability table for cdn + *)
  }

let init_state (c:code) (initcp:int) (initregs:cap_regs) (initcclock:cap_clocks) (initmem:look_mem) (initlclock:look_clocks) (initquant:quant_clocks) (initclk:int) =
  { cp = initcp;
    active = [init_thread initregs initcclock initmem initlclock initquant];
    processed = init_bpcset (size c);
    blocked = [];
    isblocked = init_pcset (size c);
    bestmatch = None;
    nextchar = 'a';             (* this won't be used before it's set *)
    clock = initclk;
    cdn = init_cdn();
  }

(** * Debugging Utilities  *)

let print_exit_allowed (b:bool) : string =
  if b then "\027[32m✔\027[0m" else "\027[31m✘\027[0m"
  
let print_thread (t:thread) : string = string_of_int t.pc ^ print_exit_allowed t.exit_allowed

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

let print_cdn_table (table:cdn_table) (cdnl:quantid list) : string =
  List.fold_left (fun str quantid ->
      let b = cdn_get table quantid in
      let s = string_of_int quantid ^ ":" ^ print_bool b in
      str ^ ", " ^ s
    ) "" cdnl
  
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
          s.active <- {pc = x; regs = t.regs; cap_clk = t.cap_clk; mem = t.mem; look_clk = t.look_clk; quants = t.quants; exit_allowed = t.exit_allowed}::s.active;
          advance_epsilon ~debug c s o
       | SetRegisterToCP r ->
          t.regs <- set_reg t.regs r s.cp; (* modifying the capture regs of the current thread *)
          t.cap_clk <- set_reg t.cap_clk r s.clock; (* saving the current clock *)
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | SetQuantToClock (q,b) ->
          let ocp = if b then (Some s.cp) else None in (* saving the current cp if we are nulling a + *)
          t.quants <- set_quant t.quants q s.clock ocp; (* adding the last iteration cp *)
          t.pc <- t.pc + 1;
          advance_epsilon ~debug c s o
       | CheckOracle l ->
          if (get_oracle o s.cp l)
          then begin
              t.pc <- t.pc + 1; (* keeping the thread alive *)
              t.mem <- set_mem t.mem l s.cp; (* remembering the cp where we last needed the oracle *)
              t.look_clk <- set_mem t.look_clk l s.clock (* remembering the clock as well *)
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
       | Fail ->
          s.active <- ac;       (* killing the current thread *)
          advance_epsilon ~debug c s o
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


(** * Null interpreter  *)
(* an interpreter that does not read the string, but instead simply follows epsilon transuitions *)
(* this is used either to build the CDN table (check if a plus is nullable at a given position) *)
(* this is also used to reconstruct the capture groups of last nulled plus at the end of a match *)

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
     
(** * Building CDN table  *)

let build_cdn (cdns:cdns) (cp:int) (o:oracle) : cdn_table =
  let table = ref (init_cdn()) in
  match cdns with
  | (cdn_codes, cdn_list) ->
     List.iter (fun qid ->
         let code = get_cdn_code cdn_codes qid in
         let init = init_state code cp (init_regs()) (init_regs()) (init_mem()) (init_mem()) (init_quant_clocks()) 0 in
         init.cdn <- !table;    (* giving it what we computed so far *)
         match (null_interp ~debug:false code init o) with
         | Some _ ->            (* if there is a match, we mark the cdn as nullable *)
            (* we can ignore what's really inside the winning thread *)
            table := cdn_set_true !table qid
         | None -> ()
       ) cdn_list;
     !table


(** * Interpreting the bytecode  *)
let rec interpreter ?(debug=false) (c:code) (str:string) (s:interpreter_state) (o:oracle) (dir:direction) (cdn:cdns): thread option =
  if debug then
    begin
      Printf.printf "%s" (print_cp s.cp);
      Printf.printf "%s" (print_active s.active);
      Printf.printf "%s%!" (print_bestmatch s.bestmatch);
    end;

  (* building the CDN table *)
  s.cdn <- build_cdn cdn s.cp o;
  if debug then
    begin
      Printf.printf "At CP%d, CDN table:%s\n" (s.cp) (print_cdn_table s.cdn (snd cdn));
    end;
  
  (* follow epsilon transitions *)  
  advance_epsilon ~debug c s o;
  if debug then
    begin
      Printf.printf "%s\n%!" (print_blocked s.blocked);
    end;
  (* checking if there are still surviving threads *)
  match s.blocked with
  | [] -> s.bestmatch
  | _ -> 
     (* read the next character *)
     let x = get_char str (s.cp - cp_offset dir)  in
     begin match x with
     | None -> s.bestmatch         (* we reached the end of the string *)
     | Some chr ->
        s.nextchar <- chr;
        (* advancing blocked threads *)
        consume ~debug c s;
        (* resetting the processed, blocked sets and the CDN table *)
        s.processed <- init_bpcset (size c); 
        s.isblocked <- init_pcset (size c);
        s.cdn <- init_cdn();
        (* advancing the current position *)
        s.cp <- incr_cp s.cp dir;
        interpreter ~debug c str s o dir cdn
     end
   
(** * Reconstructing Nullable + Values  *)
(* when the winning thread of a match decided to go through the nullable path of a +, we might need to reconstruct any groups set during that nullable path *)

let reconstruct_plus_groups ?(debug=false) ?(verbose=false) (thread:thread) (r:regex) (s:string) (o:oracle) (dir:direction): thread =
  (* let lq = nullable_plus_quantid r in (\* all the nullable + in order *\) *)
  let mem = ref thread.mem in
  let regs = ref thread.regs in
  let capclk = ref thread.cap_clk in
  let lookclk = ref thread.look_clk in
  let quants = ref thread.quants in
  (* gos through the regex, if it encounters a nulled +, it calls the null interpreter *)
  let rec nulled_plus (reg:regex) : unit =
    match reg with
    | Re_empty | Re_char _ | Re_dot -> ()
    | Re_alt (r1, r2) | Re_con (r1, r2) ->
       nulled_plus r1; nulled_plus r2
    | Re_capture (_,r1) -> nulled_plus r1
    | Re_lookaround (lid,lk,r1) -> () (* todo: check if it's ok *)
    (* from shallowest to deepest plus: *)
    | Re_quant (nul,qid,quanttype,body) ->
       begin match (get_quant_nulled !quants qid) with
       | None -> nulled_plus body (* recursive call: the inner + may have been nulled *)
       | Some start_cp ->
          (* with no recursive calls: inner + will be reconstructed automatically *)
          let start_clock = get_quant_clock !quants qid in
          let bytecode = compile_reconstruct_nulled body in
          let result = null_interp ~debug ~verbose bytecode (init_state bytecode start_cp !regs !capclk !mem !lookclk !quants start_clock) o in
          begin match result with
          | None -> failwith "expected a nullable plus"
          | Some w ->             (* there's a winning thread when nulling *)
             mem := w.mem;    (* updating the lookaround memory *)
             regs := w.regs;   (* updating the capture regs *)
             capclk := w.cap_clk; (* updating the capture clocks *)
             lookclk := w.look_clk; (* updating the lookaround clocks *)
             quants := w.quants (* updating the quantifier registers *)
          end
       end
  in
  nulled_plus r;
  {pc = thread.pc; regs = !regs; cap_clk = !capclk; mem = !mem; look_clk = !lookclk; quants = !quants; exit_allowed = thread.exit_allowed}
  
 
(** * Running the interpreter and returning its result  *)
  
(* running the interpreter on some code, with a particular initial interpreter state *)
(* also reconstructs the + groups *)
let interp ?(verbose = true) ?(debug=false) (r:regex) (c:code) (s:string) (o:oracle) (dir:direction) (start_cp:int) (start_regs:cap_regs) (start_cclock:cap_clocks) (start_mem:look_mem) (start_lclock:look_clocks) (start_quant:quant_clocks) (start_clock:int) (cdn:cdns): thread option =
  if verbose then Printf.printf "%s\n" ("\n\027[36mInterpreter:\027[0m "^s);
  if verbose then Printf.printf "%s\n" (print_code c);
  let result = interpreter ~debug c s (init_state c start_cp start_regs start_cclock start_mem start_lclock start_quant start_clock) o dir cdn in
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
  interp ~verbose ~debug r c s o dir (init_cp dir (String.length s)) (init_regs()) (init_regs()) (init_mem()) (init_mem()) (init_quant_clocks()) 0 cdn

(* for tests, sometimes we only want to know if there is a match *)
let boolean_interp ?(verbose = true) ?(debug=false) (r:regex) (c:code) (s:string) (o:oracle) (dir:direction) (cdn:cdns): bool =
  match (interp_default_init ~verbose ~debug r c s o dir cdn) with
  | None -> false
  | _ -> true



(** * Filtering With Capture Reset  *)
(* At the end of the algorithm, we get many capture groups that should be reset *)
(* We use the quantifier registers to filter out those that are too old *)

let rec filter_capture (r:regex) (regs:cap_regs ref) (cclocks: cap_clocks) (lclocks:look_clocks) (qclocks:quant_clocks) (maxclock:int) : unit =
  match r with
  | Re_empty | Re_char _ | Re_dot -> ()
  | Re_alt (r1,r2) -> filter_capture r1 regs cclocks lclocks qclocks maxclock; filter_capture r2 regs cclocks lclocks qclocks maxclock
  | Re_con (r1,r2) -> filter_capture r1 regs cclocks lclocks qclocks maxclock; filter_capture r2 regs cclocks lclocks qclocks maxclock
  | Re_quant (nul, qid, quant, r1) ->
     let quant_val = get_quant_clock qclocks qid in (* the last time we went in *)
     if (quant_val < maxclock) then
       (* the last repetition of the inner quantifier happened before the last repetition of the outer one *)
       filter_all r1 regs
     else
       filter_capture r1 regs cclocks lclocks qclocks quant_val
  | Re_capture (cid, r1) ->
     let start = get_reg cclocks (start_reg cid) in
     begin match start with
     | None -> filter_all r1 regs (* there is already no value for this capture group, we can clear everything inside *)
     | Some st ->
        if (st < maxclock)
         (* cleaning the value of group cid (and everything inside) if its value is too old *)
        then begin regs := clear_reg !regs (start_reg cid); filter_all r1 regs end
        else filter_capture r1 regs cclocks lclocks qclocks maxclock
     end
  | Re_lookaround (lid, l, r1) ->
     let look_val = get_mem lclocks lid in (* the last time we needed the lookaround to hold *)
     begin match look_val with
     | None -> filter_all r1 regs (* we didn't need the lookaround: clear everything inside *)
     | Some lookv ->
        if (lookv < maxclock)
             (* cleaning everything inside the lookaround since it's too old *)
        then filter_all r1 regs
        else filter_capture r1 regs cclocks lclocks qclocks (-1)
                            (* resetting the maxclock to -1: lookaround clocks are reset *)
     end
      
and filter_all (r:regex) (regs:cap_regs ref) : unit = (* clearing all capture group inside a regex *)
  match r with
  | Re_empty | Re_char _ | Re_dot -> ()
  | Re_alt (r1,r2) -> filter_all r1 regs; filter_all r2 regs
  | Re_con (r1,r2) -> filter_all r1 regs; filter_all r2 regs
  | Re_quant (nul, qid, quant, r1) ->
     filter_all r1 regs
  | Re_capture (cid, r1) ->
     regs := clear_reg !regs (start_reg cid);
     filter_all r1 regs
  | Re_lookaround (lid, l, r1) -> filter_all r1 regs
       

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
          
