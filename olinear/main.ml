
open Oracle
open Regex
open Bytecode
open Compiler
open Interpreter


(** * Basic Testing *)
   
let oracle_tests () =
  let o = create_oracle 4 5 in
  assert (Array.length o = 4);
  assert (Array.length (o.(0)) = 5);
  assert (get_oracle o 1 4 = false);
  set_oracle o 1 4;
  assert (get_oracle o 1 4 = true);
  assert (get_oracle o 1 3 = false)

let regex_tests () =
  let raw = Raw_con(Raw_char 'a', Raw_lookaround (Lookbehind, Raw_char 'a')) in
  Printf.printf "%s\n" (print_raw raw);
  let re = annotate raw in
  Printf.printf "%s\n" (print_regex re);
  assert (get_lookaround re 2 = None);
  assert (get_lookaround re 0 = None);
  assert (get_lookaround re 1 = Some (Re_char 'a', Lookbehind));
  let rr = reverse_regex re in
  Printf.printf "%s\n" (print_regex rr);
  assert (rr = Re_capture (0, Re_con(Re_lookaround (1, Lookbehind, Re_char 'a'), Re_char 'a')));
  let rc = remove_capture rr in
  Printf.printf "%s\n" (print_regex rc);
  assert (rc = Re_con(Re_lookaround (1, Lookbehind, Re_char 'a'), Re_char 'a'))

let bytecode_tests () =
  let bytecode = [Jmp 1; Fork (0,2); Accept] in
  Printf.printf "%s\n" (print_code bytecode);
  assert (nb_epsilon bytecode = 3)

let compiler_tests() =
  let raw = Raw_con (Raw_quant (Star, Raw_char 'a'), Raw_char 'b') in
  let re = annotate raw in
  let code = compile_to_bytecode re in
  assert (code = [SetRegisterToCP 0; Fork (2,4); Consume 'a'; Jmp 1; Consume 'b'; SetRegisterToCP 1; Accept]);
  Printf.printf "%s\n" (print_code code);
  assert(true)
  
let tests () =
  Printf.printf "\027[32mTests: \027[0m\n\n";
  oracle_tests();
  regex_tests();
  bytecode_tests();
  compiler_tests();
  Printf.printf "\027[32mTests passed\027[0m\n"

  
let main =
  tests()
