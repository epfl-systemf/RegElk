open Oracle
open Regex
open Bytecode
open Compiler
open Interpreter
open Linear
open Tojs
open Tore2
open Toexp
open Diff_fuzzer
open Complexity_exp

(** * Basic Testing *)

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
  Printf.printf "%s\n" (print_code code);
  assert (Array.to_list code = [SetRegisterToCP 0; Fork (2,5); SetQuantToClock (1,false); Consume 'a'; Jmp 1; Consume 'b'; SetRegisterToCP 1; Accept])


let compare_engines_tests() =
  ignore(compare_engines (Raw_con (Raw_quant (Star, Raw_capture (Raw_char 'a')), Raw_char 'b')) "aaab");
  ignore(compare_engines (Raw_char 'a') "b");
  ignore(compare_engines (Raw_quant (Star, Raw_alt (Raw_capture(Raw_char 'a'), Raw_capture(Raw_char 'b')))) "ababab")
  
  
(** * Gathering some errors found with the fuzzer *)
let string_sub_errors : (raw_regex*string) list = (* FIXED! reverse registers for lookbehinds groups *)
  []

let oracle_assert_errors : (raw_regex*string) list = (* FIXED! read str.cp - 1 backward *)
  [(Raw_con(Raw_lookaround(Lookbehind,Raw_empty),Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_char('a')))),"ccbba")]
  
let expected_result_oracle_errors : (raw_regex*string) list = (* FIXED! read str.cp - 1 backward *)
  [(Raw_lookaround(Lookbehind,Raw_char('b')),"bccaaacabcbcabaacbccaccbbbaaccccaabcac");
   (Raw_lookaround(Lookbehind,Raw_char('a')),"cabbcabcccacbbabcb")]

let idk : (raw_regex*string) list = (* FIXED, but i don't know why *)
  []
  
let clear_mem : (raw_regex*string) list = (* FIXED! clear the lookaround memory in quantifiers *)
  [] 

let empty_problem : (raw_regex*string) list = (* FIXED, by compiling Plus as Concatenation with Star *)
  [(Raw_con(Raw_lookaround(NegLookbehind,Raw_char('b')),Raw_quant(Plus,Raw_alt(Raw_empty,Raw_capture(Raw_dot)))),"bbccbacbccbcabbcbcaccccba");
   (Raw_alt(Raw_quant(Plus,Raw_alt(Raw_empty,Raw_dot)),Raw_capture(Raw_alt(Raw_dot,Raw_char('c')))),"bbccacccabcbaaabbabbbccccccacbaabacbc")]

let double_quant : (raw_regex*string) list = (* FIXED, with another way to compile lazystar *)
  [(Raw_quant(Plus,Raw_alt(Raw_quant(LazyStar,Raw_lookaround(NegLookbehind,Raw_con(Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookbehind,Raw_alt(Raw_char('c'),Raw_char('c')))),Raw_lookaround(NegLookbehind,Raw_quant(Plus,Raw_dot))))),Raw_dot)),"abacaaaacabaccbcabcacabccbcaacbabaa");
   (Raw_capture(Raw_quant(Plus,Raw_quant(LazyStar,Raw_dot))),"bacababaacbcabaabccccacca"); 
   (Raw_con(Raw_con(Raw_empty,Raw_capture(Raw_quant(Star,Raw_quant(LazyStar,Raw_dot)))),Raw_dot),"abcbbca")]

let empty_group : (raw_regex*string) list =
  [(Raw_quant(Star,Raw_alt(Raw_con(Raw_char('a'),Raw_capture(Raw_empty)),Raw_char('b'))),"ab")]


(* FIXED by preventing advance_epsilon from calling itself twice *)
let should_not_clear : (raw_regex*string) list =
  [(Raw_capture(Raw_capture(Raw_quant(Plus,Raw_capture(Raw_con(Raw_con(Raw_quant(LazyPlus,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookbehind,Raw_char('b')))),Raw_con(Raw_char('c'),Raw_empty)),Raw_capture(Raw_alt(Raw_con(Raw_quant(LazyPlus,Raw_empty),Raw_capture(Raw_capture(Raw_quant(LazyPlus,Raw_empty)))),Raw_capture(Raw_dot)))))))),"ccbbbccababcababbbaccabccbacababbacbcababcabbaccccacbccbbbcbbcbcacacbaaacaaababccbbbbbbac")]

(* here we agree with Experimental, but Experimental does not agree with Irregexp! *)
(* The problem here is that we merge threads that may have a different future according to the JS semantics *)
let empty_repetitions : (raw_regex*string) list =
  [(Raw_quant(Plus,Raw_con(Raw_alt(Raw_char('a'),Raw_empty),Raw_quant(LazyStar,Raw_capture(Raw_dot)))),"bab");
   (Raw_quant(Star,Raw_con(Raw_alt(Raw_char('a'),Raw_empty),Raw_quant(LazyStar,Raw_dot))),"ab"); (* simplified example *)
   (Raw_quant(Star,Raw_con(Raw_alt(Raw_char('a'),Raw_empty),Raw_alt(Raw_empty,Raw_char('b')))),"ab")] (* no lazy stars, just alternation *)

(* FIXED by preventing advance_epsilon from calling itself twice *)
let linear_stuck : (raw_regex*string) list =
  []

(* bugs when I switched to linear compilation of the nullable + *)
(* Fixed the first 2 by starting the original thread with a true for exit_allowed, otherwise it fails to take empty Plusses *)
(* But the last one is still a bug *)
let linear_plus : (raw_regex*string) list =
  []


(* Fixed: we now reconstruct the empty groups inside the nullable plus *)
let plus_reconstruct : (raw_regex*string) list =
  [(Raw_con(Raw_quant(Plus,Raw_capture(Raw_empty)),Raw_char('a')),"a");
   (Raw_quant(LazyPlus,Raw_con(Raw_quant(LazyPlus,Raw_capture(Raw_dot)),Raw_con(Raw_alt(Raw_quant(Plus,Raw_capture(Raw_empty)),Raw_quant(Star,Raw_quant(Star,Raw_char('c')))),Raw_alt(Raw_lookaround(NegLookbehind,Raw_empty),Raw_alt(Raw_capture(Raw_capture(Raw_empty)),Raw_char('c')))))),"abaabaccbcabaccabaacabccabbccacbbccbcbacabaacbaaacbacabbaacabaccabaacbbbbccaccaacaacabccccba");
   (Raw_quant(Plus,Raw_capture(Raw_capture(Raw_capture(Raw_capture(Raw_con(Raw_empty,Raw_empty)))))),"cccccacaccbccabbcabacbaacacabcacbbabcbcccacbcab");
   (Raw_quant(Plus,Raw_capture(Raw_quant(LazyStar,Raw_empty))),"ccbcbbbbcacbcabbccaaccaccaacacabaacbbbcccbbaabaabccbacccac")]

(* more cin examples  *)
let cin_examples : (raw_regex*string) list =
  []

(* FIXED. bugs when I did not construct the CDN table yet *)
let cdn_empty: (raw_regex*string) list =
  [(Raw_quant(Plus,Raw_con(Raw_empty,Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_quant(Plus,Raw_char('c'))))))),"bbaaaacaabaabbbaaacacaacbccbacaacaaaaaabccbbbbbcbcccacabbbaabccccacccabacccacbcbbaacbccbcaacb");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_char('c'))),"ababaaaacbcaababaaccaccacbbbccabbabbcacaa");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_char('b'))),"baabacaccabacabcaaacbbbabacaabbcbbcbaccccaacbacbbababcccbcccacbabababcabbaabacbbbbcaaa")]

(* fails the assertion "expected a nullable plus" *)
(* FIXED, when we don't forget to build a CDN table when reconstructing the + groups *)
let nullable_expected: (raw_regex*string) list =
  [(Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_con(Raw_char('a'),Raw_con(Raw_char('b'),Raw_char('b'))))))),"bcaaabccbaabccaaaaababbaaaaaaaaaacbabacabcbbcbbaaabcbbaccabccbcacacc")]

(* testing the CDN formulas *)
let cdn_formulas: (raw_regex*string) list =
  []
  
(* JS is stuck (timeout), but not our engine *)
let redos : (raw_regex*string) list =
  []
  
(* re-checking a list of previous bugs *)
let replay_bugs (l:(raw_regex*string) list) =
  List.iter (fun (raw,str) -> ignore(compare_engines raw str)) l

(* just checking that our engine is not stuck on the REDOS regexes *)
let replay_stuck (l:(raw_regex*string) list) =
  List.iter (fun (raw,str) -> ignore(full_match raw str)) l

(** * Running tests  *)
let tests () =
  Printf.printf "\027[32mTests: \027[0m\n\n";
  regex_tests();
  bytecode_tests();
  compiler_tests();
  compare_engines_tests();
  replay_bugs(string_sub_errors);
  replay_bugs(oracle_assert_errors);
  replay_bugs(expected_result_oracle_errors);
  replay_bugs(idk);
  replay_bugs(clear_mem);
  replay_bugs(empty_problem);
  replay_bugs(double_quant);
  replay_bugs(empty_group);
  replay_bugs(should_not_clear);
  replay_bugs(empty_repetitions);
  replay_bugs(linear_stuck);
  replay_bugs(linear_plus);
  replay_bugs(plus_reconstruct);
  replay_bugs(cin_examples);
  replay_bugs(cdn_empty);
  replay_bugs(nullable_expected);
  replay_bugs(cdn_formulas);
  replay_stuck(redos);
  Printf.printf "\027[32mTests passed\027[0m\n"


let main =
  (* tests() *)
  fuzzer()
  (* run_benchmark(many_forks); *)
  (* many_forks_re2_benchmark() *)

  (* TODO: make more JSCore benchmarks *)
  (* let open Core in
   *   let open Core_bench in
   *   let (array_args,matcher_fn,name) = prepare_core_benchmark quadratic_plus in
   *   Command_unix.run (Bench.make_command [
   *                    Bench.Test.create_indexed
   *                      ~name
   *                      ~args:(List.init (Array.length array_args) (fun i -> i))
   *                      matcher_fn ]) *)
