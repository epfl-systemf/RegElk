
open Oracle
open Regex
open Bytecode
open Compiler
open Interpreter
open Linear
open Tojs
open Diff_fuzzer

(** * Basic Testing *)
   
let oracle_tests () =
  let o = create_oracle 4 8 in
  assert (Array.length o = 5);
  assert (Array.length (o.(0)) = 8);
  assert (get_oracle o 1 4 = false);
  set_oracle o 1 4;
  Printf.printf "%s\n" (print_oracle o);
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
  let code = [Jmp 1; Fork (0,2); Accept] in
  let bytecode = Array.of_list code in
  Printf.printf "%s\n" (print_code bytecode);
  assert (nb_epsilon bytecode = 3)

let compiler_tests () =
  let raw = Raw_con (Raw_quant (Star, Raw_char 'a'), Raw_char 'b') in
  let re = annotate raw in
  let code = compile_to_bytecode re in
  assert (Array.to_list code = [SetRegisterToCP 0; Fork (2,4); Consume 'a'; Jmp 1; Consume 'b'; SetRegisterToCP 1; Accept]);
  Printf.printf "%s\n" (print_code code);
  assert(true)

let interpreter_tests () =
  let o = create_oracle 1 1 in
  let raw = Raw_con (Raw_quant (Star, Raw_char 'a'), Raw_char 'b') in
  let re = annotate raw in
  let code = compile_to_bytecode re in
  let str1 = "aab" in
  let str2 = "aaa" in
  assert (match_interp ~debug:true code str1 o Forward = true);
  assert (match_interp ~debug:true code str2 o Forward = false)

let build_oracle_tests () =
  let raw = Raw_con(Raw_con (Raw_lookaround (Lookahead, Raw_char 'a'), Raw_lookaround (Lookbehind, Raw_con (Raw_char 'a',Raw_char 'b'))), Raw_lookaround(Lookbehind, Raw_empty)) in
  let re = annotate raw in
  let str = "aaab" in
  Printf.printf "%s\n" (print_regex re);
  let o = build_oracle ~debug:true re str in
  Printf.printf "%s\n" (print_oracle o);
  assert (get_oracle o 4 2 = true);
  assert (get_oracle o 3 2 = false);
  assert (get_oracle o 2 1 = true);
  assert (get_oracle o 2 0 = false);
  assert (get_oracle o 4 1 = false)

let full_algo_tests () =
  let raw = Raw_con (Raw_char 'a', Raw_lookaround (Lookahead, Raw_capture(Raw_char 'b'))) in
  let str = "cab" in
  ignore(full_match ~verbose:true ~debug:true raw str)

let compare_engines_tests() =
  compare_engines (Raw_con (Raw_quant (Star, Raw_capture (Raw_char 'a')), Raw_char 'b')) "aaab";
  compare_engines (Raw_char 'a') "b";
  compare_engines (Raw_quant (Star, Raw_alt (Raw_capture(Raw_char 'a'), Raw_capture(Raw_char 'b')))) "ababab"
  
  
(** * Gathering some errors found with the fuzzer *)
let string_sub_errors : (raw_regex*string) list = (* FIXED! reverse registers for lookbehinds groups *)
  [(Raw_lookaround(Lookbehind,Raw_capture(Raw_dot)),"bababaaabbacacbabbacabcccaaacaabccab");
   (Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_capture(Raw_capture(Raw_alt(Raw_empty,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookahead,Raw_quant(LazyStar,Raw_alt(Raw_lookaround(NegLookahead,Raw_dot),Raw_capture(Raw_dot)))))))),Raw_lookaround(Lookbehind,Raw_capture(Raw_dot))))),"bcacaaaacaabcbbcacaaacbbaabc");
   (Raw_lookaround(Lookbehind,Raw_capture(Raw_dot)),"cabacbbccbacbcbbccbaccbaccabbbaaa");
   (Raw_lookaround(Lookbehind,Raw_capture(Raw_capture(Raw_dot))),"bbacccabbcccbcccaabcbabcaaacacacbbbcabbc")]

let oracle_assert_errors : (raw_regex*string) list = (* FIXED! read str.cp - 1 backward *)
  [(Raw_con(Raw_lookaround(Lookbehind,Raw_empty),Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_char('a')))),"ccbba")]
  
let expected_result_oracle_errors : (raw_regex*string) list = (* FIXED! read str.cp - 1 backward *)
  [(Raw_lookaround(Lookbehind,Raw_char('b')),"bccaaacabcbcabaacbccaccbbbaaccccaabcac");
   (Raw_lookaround(Lookbehind,Raw_char('a')),"cabbcabcccacbbabcb")]

let idk : (raw_regex*string) list = (* FIXED, but i don't know why *)
  [(Raw_quant(Plus,Raw_con(Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_con(Raw_char('b'),Raw_empty),Raw_capture(Raw_empty)))),Raw_empty)),"bbacaaaaccbcaaccaacaaababacccbcbbbccbccb");
   (Raw_capture(Raw_con(Raw_con(Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_alt(Raw_alt(Raw_char('b'),Raw_capture(Raw_empty)),Raw_char('a')))),Raw_quant(Star,Raw_lookaround(NegLookbehind,Raw_capture(Raw_lookaround(NegLookahead,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_char('c')))))))),Raw_quant(Plus,Raw_con(Raw_dot,Raw_empty)))),"babcbcaacccbbcccabacaccaaccbabcbbbabbabbbabbcbcaa")]
  
let clear_mem : (raw_regex*string) list = (* FIXED! clear the lookaround memory in quantifiers *)
  [(Raw_quant(Star,Raw_alt(Raw_con(Raw_char('a'),Raw_lookaround(Lookahead,Raw_capture(Raw_char('b')))),Raw_char('b'))),"abc")] 

let double_quant : (raw_regex*string) list = (* not fixed. not sure what we should do. maybe related to the empty problem *)
  [(Raw_quant(Plus,Raw_alt(Raw_quant(LazyStar,Raw_lookaround(NegLookbehind,Raw_con(Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookbehind,Raw_alt(Raw_char('c'),Raw_char('c')))),Raw_lookaround(NegLookbehind,Raw_quant(Plus,Raw_dot))))),Raw_dot)),"abacaaaacabaccbcabcacabccbcaacbabaa");
   (Raw_capture(Raw_quant(Plus,Raw_quant(LazyStar,Raw_dot))),"bacababaacbcabaabccccacca"); 
   (Raw_con(Raw_con(Raw_empty,Raw_capture(Raw_quant(Star,Raw_quant(LazyStar,Raw_dot)))),Raw_dot),"")]

let empty_problem : (raw_regex*string) list = (* not fixed. the problem comes from Raw_empty. If we replace it with dot, no issues *)
  [(Raw_con(Raw_lookaround(NegLookbehind,Raw_char('b')),Raw_quant(Plus,Raw_alt(Raw_empty,Raw_capture(Raw_dot)))),"bbccbacbccbcabbcbcaccccba");
   (Raw_quant(Plus,Raw_alt(Raw_empty,Raw_con(Raw_alt(Raw_dot,Raw_alt(Raw_dot,Raw_empty)),Raw_alt(Raw_lookaround(Lookahead,Raw_char('b')),Raw_quant(LazyStar,Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_capture(Raw_empty),Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_con(Raw_con(Raw_dot,Raw_capture(Raw_dot)),Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_capture(Raw_quant(LazyStar,Raw_empty))))))))))))))),"bbccaaccbbbaccabccbabccababc");
   (Raw_alt(Raw_quant(Plus,Raw_alt(Raw_empty,Raw_dot)),Raw_capture(Raw_alt(Raw_dot,Raw_char('c')))),"bbccacccabcbaaabbabbbccccccacbaabacbc")]

let different_results : (raw_regex*string) list =
  []


(* JS is stuck, but not our engine *)
let stuck : (raw_regex*string) list =
  [(Raw_lookaround(Lookbehind,Raw_con(Raw_lookaround(NegLookahead,Raw_dot),Raw_quant(LazyPlus,Raw_capture(Raw_con(Raw_quant(Star,Raw_char('a')),Raw_con(Raw_alt(Raw_empty,Raw_dot),Raw_dot)))))),"cbabbccccbbcccaaaaaccabccbaabaabcaaacbca")]
  
let replay_bugs (l:(raw_regex*string) list) =
  List.iter (fun (raw,str) -> compare_engines raw str) l


(** * Running tests  *)
let tests () =
  Printf.printf "\027[32mTests: \027[0m\n\n";
  oracle_tests();
  regex_tests();
  bytecode_tests();
  compiler_tests();
  interpreter_tests();
  build_oracle_tests();
  full_algo_tests();
  compare_engines_tests();
  replay_bugs(oracle_assert_errors);
  replay_bugs(expected_result_oracle_errors);
  replay_bugs(string_sub_errors);
  replay_bugs(idk);
  replay_bugs(clear_mem);
  Printf.printf "\027[32mTests passed\027[0m\n"

  
let main =
  (* tests(); *)
  (* fuzzer() *)
  compare_engines (Raw_alt(Raw_quant(Plus,Raw_alt(Raw_dot,Raw_dot)),Raw_capture(Raw_alt(Raw_dot,Raw_char('c'))))) "bbccacccabcbaaabbabbbccccccacbaabacbc"
  
    
