(** * Regex Bytecode  *)
(* The code corresponding to a bytecode that the VM should execute in a lockstep fashion (Thompson simulation) *)

open Regex

(* corresponding to indices in a list of instructions *)
type label = int

(* capture registers *)
type register = int

(* when the next label isn't directly specified, we expect a falltrough order of just going to the next instruction in the list *)
type instruction =
  | Consume of char
  | ConsumeAll                  (* fot the dot. TODO: generalize to ranges *)
  | Accept
  | Jmp of label
  | Fork of label * label
  | SetRegisterToCP of register
  | ClearRegister of register
  | ClearMemory of lookid       (* clears the lookaround mem for quantifiers *)
  | CheckOracle of lookid       (* checks the oracle at the current CP. Kills the thread on a failure. Writes to the lookaround mem *)
  | NegCheckOracle of lookid    (* same, but expects a negative answer *)
  | WriteOracle of lookid       (* when we find a match, we write to the oracle at CP *)
  | BeginLoop                   (* start of loop: we set a counter to prevent exiting it using only epsilon transitions *)
  | EndLoop                     (* end of loop: fails if we started the loop without consuming in the string *)
                     (* Missing instruction from Experimental: 0-width assertion *)

type code = instruction Array.t
(* TODO: a list has random access complexity O(n) *)
(* I want a random access complexity of O(1) *)
(* Because the code is accessed for each thread at a different pc *)
(* This is why we use an array instead of a list *)

let get_instr (c:code) (pc:label) : instruction =
  Array.get c pc
           (* this will probably change to an array access for O(1) complexity *)

let size (c:code) : int =
  Array.length c
  
(** * Bytecode Properties *)
          
(* Counting epsilon transitions. This will be useful to provide a bound when finding the next active thread list *)
let nb_epsilon_transition (i:instruction) : int =
  match i with
  | Fork _ -> 2
  | Jmp _ | CheckOracle _ | NegCheckOracle _ -> 1
  | _ -> 0 
           
let nb_epsilon (c:code) : int =
  Array.fold_left (fun n i -> n + (nb_epsilon_transition i)) 0 c


(** * Bytecode Pretty-printing *)

let print_instruction (i:instruction) : string =
  match i with
  | Consume ch -> "Consume " ^ String.make 1 ch
  | ConsumeAll -> "ConsumeAll"
  | Accept -> "Accept"
  | Jmp l -> "Jmp " ^ string_of_int l
  | Fork (l1,l2) -> "Fork " ^ string_of_int l1 ^ " " ^ string_of_int l2
  | SetRegisterToCP r -> "SetRegisterToCP " ^ string_of_int r
  | ClearRegister r -> "ClearRegister " ^ string_of_int r
  | ClearMemory l -> "ClearMemory " ^ string_of_int l
  | CheckOracle l -> "CheckOracle " ^ string_of_int l
  | NegCheckOracle l -> "NegCheckOracle " ^ string_of_int l
  | WriteOracle l -> "WriteOracle " ^ string_of_int l
  | BeginLoop -> "BeginLoop"
  | EndLoop -> "EndLoop"
  
let rec print_code (c:code) : string =
  let s = ref "" in
  for i=0 to (size c)-1 do
    s := !s ^ "\027[33m" ^ string_of_int i ^ ":\027[0m " ^ print_instruction (get_instr c i) ^ "\n"
  done;
  !s
                               
