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
  | Accept                      (* has different effects depending on the stage of the algorithm *)
  | Jmp of label
  | Fork of label * label
  | SetRegisterToCP of register
  | ClearRegister of register
  | CheckOracle of lookid       (* checks the oracle at the current CP. Kills the thread on a failure *)
                     (* Missing instruction from Experimental: 0-width assertion *)

type code = instruction list
(* TODO: a list has random access complexity O(n) *)
(* I want a random access complexity of O(1) *)
(* Because the code is accessed for each thread at a different pc *)
(* Maybe I should instead put the code in an array of instructions? *)

(** * Bytecode Properties *)
          
(* Counting epsilon transitions. This will be useful to provide a bound when finding the next active thread list *)
let nb_epsilon_transition (i:instruction) : int =
  match i with
  | Fork _ -> 2
  | Jmp _ -> 1
  | _ -> 0 (* TODO: should CheckOracle count as an Epsilon Transition? *)
           
let nb_epsilon (c:code) : int =
  List.fold_left (fun n i -> n + (nb_epsilon_transition i)) 0 c


(** * Bytecode Pretty-printing *)

let print_instruction (i:instruction) : string =
  match i with
  | Consume ch -> String.make 1 ch
  | Accept -> "Accept"
  | Jmp l -> "Jmp " ^ string_of_int l
  | Fork (l1,l2) -> "Fork " ^ string_of_int l1 ^ " " ^ string_of_int l2
  | SetRegisterToCP r -> "SetRegisterToCP " ^ string_of_int r
  | ClearRegister r -> "ClearRegister " ^ string_of_int r
  | CheckOracle l -> "CheckOracle " ^ string_of_int l
  
let rec print_code (c:code) (pc:int) : string =
  match c with
  | [] -> ""
  | i::c' -> "\027[33m" ^ string_of_int pc ^ ":\027[0m " ^print_instruction i ^ "\n" ^ print_code c' (pc+1)
     
let print_code (c:code) : string = print_code c 0
