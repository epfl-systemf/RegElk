open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Interpreter
open Linear
open Tojs
open Tore2
open Toexp
open Diff_fuzzer
open Complexity_exp

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
  Printf.printf "%s\n" (print_code code);
  assert (Array.to_list code = [SetRegisterToCP 0; Fork (2,5); SetQuantToClock (1,false); Consume 'a'; Jmp 1; Consume 'b'; SetRegisterToCP 1; Accept])

let interpreter_tests () =
  let o = create_oracle 1 1 in
  let raw = Raw_con (Raw_quant (Star, Raw_char 'a'), Raw_char 'b') in
  let re = annotate raw in
  let code = compile_to_bytecode re in
  let cdn = compile_cdns re in
  let str1 = "aab" in
  let str2 = "aaa" in
  assert (boolean_interp ~debug:true re code str1 o Forward cdn = true);
  assert (boolean_interp ~debug:true re code str2 o Forward cdn = false)

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
  ignore(compare_engines (Raw_con (Raw_quant (Star, Raw_capture (Raw_char 'a')), Raw_char 'b')) "aaab");
  ignore(compare_engines (Raw_char 'a') "b");
  ignore(compare_engines (Raw_quant (Star, Raw_alt (Raw_capture(Raw_char 'a'), Raw_capture(Raw_char 'b')))) "ababab")
  
  
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
  [(Raw_capture(Raw_con(Raw_con(Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_alt(Raw_alt(Raw_char('b'),Raw_capture(Raw_empty)),Raw_char('a')))),Raw_quant(Star,Raw_lookaround(NegLookbehind,Raw_capture(Raw_lookaround(NegLookahead,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_char('c')))))))),Raw_quant(Plus,Raw_con(Raw_dot,Raw_empty)))),"babcbcaacccbbcccabacaccaaccbabcbbbabbabbbabbcbcaa")]
  
let clear_mem : (raw_regex*string) list = (* FIXED! clear the lookaround memory in quantifiers *)
  [(Raw_quant(Star,Raw_alt(Raw_con(Raw_char('a'),Raw_lookaround(Lookahead,Raw_capture(Raw_char('b')))),Raw_char('b'))),"abc")] 

let empty_problem : (raw_regex*string) list = (* FIXED, by compiling Plus as Concatenation with Star *)
  [(Raw_con(Raw_lookaround(NegLookbehind,Raw_char('b')),Raw_quant(Plus,Raw_alt(Raw_empty,Raw_capture(Raw_dot)))),"bbccbacbccbcabbcbcaccccba");
   (Raw_quant(Plus,Raw_alt(Raw_empty,Raw_con(Raw_alt(Raw_dot,Raw_alt(Raw_dot,Raw_empty)),Raw_alt(Raw_lookaround(Lookahead,Raw_char('b')),Raw_quant(LazyStar,Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_capture(Raw_empty),Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_con(Raw_con(Raw_dot,Raw_capture(Raw_dot)),Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_capture(Raw_quant(LazyStar,Raw_empty))))))))))))))),"bbccaaccbbbaccabccbabccababc");
   (Raw_alt(Raw_quant(Plus,Raw_alt(Raw_empty,Raw_dot)),Raw_capture(Raw_alt(Raw_dot,Raw_char('c')))),"bbccacccabcbaaabbabbbccccccacbaabacbc")]

let double_quant : (raw_regex*string) list = (* FIXED, with another way to compile lazystar *)
  [(Raw_quant(Plus,Raw_alt(Raw_quant(LazyStar,Raw_lookaround(NegLookbehind,Raw_con(Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookbehind,Raw_alt(Raw_char('c'),Raw_char('c')))),Raw_lookaround(NegLookbehind,Raw_quant(Plus,Raw_dot))))),Raw_dot)),"abacaaaacabaccbcabcacabccbcaacbabaa");
   (Raw_capture(Raw_quant(Plus,Raw_quant(LazyStar,Raw_dot))),"bacababaacbcabaabccccacca"); 
   (Raw_con(Raw_con(Raw_empty,Raw_capture(Raw_quant(Star,Raw_quant(LazyStar,Raw_dot)))),Raw_dot),"abcbbca")]

let empty_group : (raw_regex*string) list =
  [(Raw_quant(Star,Raw_alt(Raw_con(Raw_char('a'),Raw_capture(Raw_empty)),Raw_char('b'))),"ab")]


(* FIXED by preventing advance_epsilon from calling itself twice *)
let should_not_clear : (raw_regex*string) list =
  [(Raw_alt(Raw_con(Raw_quant(LazyStar,Raw_alt(Raw_capture(Raw_con(Raw_quant(Star,Raw_lookaround(Lookahead,Raw_empty)),Raw_empty)),Raw_capture(Raw_empty))),Raw_char('a')),Raw_empty),"cbacacaabbcbaacbbababcbcaaa");
   (Raw_alt(Raw_con(Raw_alt(Raw_empty,Raw_lookaround(NegLookbehind,Raw_lookaround(Lookbehind,Raw_quant(LazyPlus,Raw_char('b'))))),Raw_lookaround(Lookahead,Raw_char('b'))),Raw_quant(LazyStar,Raw_char('c'))),"cbcaaaacccabbaacccaacbcbacbbabcaccbbbbc");
   (Raw_con(Raw_capture(Raw_capture(Raw_con(Raw_con(Raw_alt(Raw_capture(Raw_con(Raw_alt(Raw_quant(LazyStar,Raw_dot),Raw_capture(Raw_lookaround(NegLookbehind,Raw_capture(Raw_lookaround(NegLookahead,Raw_char('c')))))),Raw_capture(Raw_char('b')))),Raw_quant(Plus,Raw_con(Raw_char('a'),Raw_alt(Raw_con(Raw_lookaround(NegLookbehind,Raw_lookaround(Lookbehind,Raw_capture(Raw_dot))),Raw_empty),Raw_con(Raw_capture(Raw_lookaround(NegLookbehind,Raw_quant(LazyStar,Raw_dot))),Raw_char('a')))))),Raw_char('b')),Raw_con(Raw_con(Raw_con(Raw_dot,Raw_dot),Raw_lookaround(NegLookahead,Raw_capture(Raw_char('c')))),Raw_capture(Raw_con(Raw_capture(Raw_dot),Raw_dot)))))),Raw_alt(Raw_con(Raw_alt(Raw_lookaround(NegLookbehind,Raw_alt(Raw_char('a'),Raw_empty)),Raw_quant(LazyPlus,Raw_empty)),Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_lookaround(NegLookahead,Raw_con(Raw_con(Raw_lookaround(Lookahead,Raw_char('c')),Raw_lookaround(NegLookbehind,Raw_capture(Raw_alt(Raw_capture(Raw_empty),Raw_lookaround(NegLookbehind,Raw_char('b')))))),Raw_char('a'))))))),Raw_empty)),"cabababbccaaccbabbaaacbb");
   (Raw_capture(Raw_capture(Raw_quant(Plus,Raw_capture(Raw_con(Raw_con(Raw_quant(LazyPlus,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookbehind,Raw_char('b')))),Raw_con(Raw_char('c'),Raw_empty)),Raw_capture(Raw_alt(Raw_con(Raw_quant(LazyPlus,Raw_empty),Raw_capture(Raw_capture(Raw_quant(LazyPlus,Raw_empty)))),Raw_capture(Raw_dot)))))))),"ccbbbccababcababbbaccabccbacababbacbcababcabbaccccacbccbbbcbbcbcacacbaaacaaababccbbbbbbac")]

(* here we agree with Experimental, but Experimental does not agree with Irregexp! *)
(* The problem here is that we merge threads that may have a different future according to the JS semantics *)
let empty_repetitions : (raw_regex*string) list =
  [(Raw_quant(Plus,Raw_con(Raw_alt(Raw_char('a'),Raw_empty),Raw_quant(LazyStar,Raw_capture(Raw_dot)))),"bab");
   (Raw_quant(Star,Raw_con(Raw_alt(Raw_char('a'),Raw_empty),Raw_quant(LazyStar,Raw_dot))),"ab"); (* simplified example *)
   (Raw_quant(Star,Raw_con(Raw_alt(Raw_char('a'),Raw_empty),Raw_alt(Raw_empty,Raw_char('b')))),"ab")] (* no lazy stars, just alternation *)

(* FIXED by preventing advance_epsilon from calling itself twice *)
let linear_stuck : (raw_regex*string) list =
  [(Raw_con(Raw_lookaround(Lookbehind,Raw_empty),Raw_capture(Raw_con(Raw_dot,Raw_quant(Plus,Raw_capture(Raw_con(Raw_lookaround(NegLookbehind,Raw_quant(LazyPlus,Raw_quant(LazyPlus,Raw_quant(LazyPlus,Raw_capture(Raw_alt(Raw_capture(Raw_alt(Raw_alt(Raw_dot,Raw_alt(Raw_lookaround(NegLookbehind,Raw_alt(Raw_empty,Raw_capture(Raw_empty))),Raw_con(Raw_alt(Raw_capture(Raw_dot),Raw_capture(Raw_char('b'))),Raw_empty))),Raw_quant(LazyStar,Raw_capture(Raw_con(Raw_empty,Raw_alt(Raw_capture(Raw_quant(Plus,Raw_capture(Raw_capture(Raw_capture(Raw_con(Raw_alt(Raw_empty,Raw_empty),Raw_capture(Raw_capture(Raw_con(Raw_capture(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_empty))),Raw_con(Raw_quant(Star,Raw_capture(Raw_con(Raw_alt(Raw_empty,Raw_capture(Raw_alt(Raw_capture(Raw_quant(LazyPlus,Raw_empty)),Raw_alt(Raw_capture(Raw_char('a')),Raw_dot)))),Raw_alt(Raw_char('c'),Raw_empty)))),Raw_char('b'))))))))))),Raw_con(Raw_lookaround(NegLookbehind,Raw_capture(Raw_quant(Star,Raw_lookaround(NegLookbehind,Raw_lookaround(Lookahead,Raw_con(Raw_capture(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_lookaround(Lookahead,Raw_empty)))),Raw_dot)))))),Raw_empty))))))),Raw_lookaround(NegLookahead,Raw_empty))))))),Raw_empty)))))),"cbacaaaababbcaaaaababcabcabaccaaaacb")]

(* bugs when I switched to linear compilation of the nullable + *)
(* Fixed the first 2 by starting the original thread with a true for exit_allowed, otherwise it fails to take empty Plusses *)
(* But the last one is still a bug *)
let linear_plus : (raw_regex*string) list =
  [(Raw_alt(Raw_lookaround(NegLookbehind,Raw_capture(Raw_capture(Raw_con(Raw_quant(Star,Raw_empty),Raw_capture(Raw_quant(Plus,Raw_alt(Raw_capture(Raw_empty),Raw_lookaround(NegLookbehind,Raw_capture(Raw_capture(Raw_con(Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_capture(Raw_quant(LazyStar,Raw_alt(Raw_lookaround(Lookbehind,Raw_con(Raw_char('c'),Raw_dot)),Raw_dot))),Raw_empty))),Raw_quant(Star,Raw_lookaround(Lookahead,Raw_lookaround(Lookbehind,Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_quant(LazyStar,Raw_quant(LazyPlus,Raw_empty)),Raw_quant(LazyStar,Raw_capture(Raw_capture(Raw_empty)))))))))))))))))))),Raw_capture(Raw_empty)),"bacabacbcabcaacac");
   (Raw_quant(Plus,Raw_con(Raw_capture(Raw_lookaround(Lookbehind,Raw_alt(Raw_con(Raw_char('b'),Raw_empty),Raw_capture(Raw_empty)))),Raw_empty)),"bbacaaaaccbcaaccaacaaababacccbcbbbccbccb");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_alt(Raw_dot,Raw_capture(Raw_empty)))),"b"); (* simplified *)
   (Raw_alt(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_con(Raw_lookaround(NegLookahead,Raw_capture(Raw_empty)),Raw_lookaround(Lookahead,Raw_capture(Raw_dot)))))),Raw_lookaround(Lookahead,Raw_con(Raw_capture(Raw_alt(Raw_quant(Star,Raw_capture(Raw_con(Raw_alt(Raw_lookaround(Lookahead,Raw_char('b')),Raw_lookaround(Lookahead,Raw_char('c'))),Raw_capture(Raw_alt(Raw_lookaround(NegLookbehind,Raw_alt(Raw_lookaround(Lookbehind,Raw_char('c')),Raw_char('c'))),Raw_capture(Raw_con(Raw_dot,Raw_dot))))))),Raw_con(Raw_dot,Raw_alt(Raw_dot,Raw_char('b'))))),Raw_quant(Star,Raw_con(Raw_quant(Plus,Raw_capture(Raw_lookaround(NegLookbehind,Raw_char('b')))),Raw_quant(Star,Raw_con(Raw_dot,Raw_dot))))))),"accababbbabbccacabcaccaabcbbabcaaacbaaccabacababa")]

(* Fixed: we now reconstruct the empty groups inside the nullable plus *)
let plus_reconstruct : (raw_regex*string) list =
  [(Raw_con(Raw_quant(Plus,Raw_capture(Raw_empty)),Raw_char('a')),"a");
   (Raw_quant(Plus,Raw_alt(Raw_con(Raw_lookaround(Lookbehind,Raw_lookaround(NegLookahead,Raw_empty)),Raw_capture(Raw_char('b'))),Raw_alt(Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_alt(Raw_empty,Raw_dot))),Raw_alt(Raw_lookaround(NegLookbehind,Raw_dot),Raw_empty)),Raw_char('c')))),"babcaccbbabcacacabcaaaababbccaccccbabbcabccbbbcaacbbababccabacbbbabcbacbaabcbbccbabbccbaa");
   (Raw_quant(LazyPlus,Raw_con(Raw_quant(LazyPlus,Raw_capture(Raw_dot)),Raw_con(Raw_alt(Raw_quant(Plus,Raw_capture(Raw_empty)),Raw_quant(Star,Raw_quant(Star,Raw_char('c')))),Raw_alt(Raw_lookaround(NegLookbehind,Raw_empty),Raw_alt(Raw_capture(Raw_capture(Raw_empty)),Raw_char('c')))))),"abaabaccbcabaccabaacabccabbccacbbccbcbacabaacbaaacbacabbaacabaccabaacbbbbccaccaacaacabccccba");
   (Raw_quant(Plus,Raw_capture(Raw_capture(Raw_capture(Raw_capture(Raw_con(Raw_empty,Raw_empty)))))),"cccccacaccbccabbcabacbaacacabcacbbabcbcccacbcab");
   (Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_empty,Raw_quant(Plus,Raw_capture(Raw_empty))))),"a");
   (Raw_quant(Plus,Raw_capture(Raw_quant(LazyStar,Raw_empty))),"ccbcbbbbcacbcabbccaaccaccaacacabaacbbbcccbbaabaabccbacccac")]

(* more cin examples  *)
let cin_examples : (raw_regex*string) list =
  [(Raw_con(Raw_quant(Plus,Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_empty)),Raw_alt(Raw_empty,Raw_char('a')))),Raw_char('b')),"b");
   (Raw_con(Raw_quant(Star,Raw_quant(Plus,Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_empty)),Raw_alt(Raw_empty,Raw_char('a'))))),Raw_char('b')),"ab");
   (Raw_con(Raw_quant(Star,Raw_con(Raw_quant(Plus,Raw_alt(Raw_lookaround(Lookahead,Raw_capture(Raw_empty)),Raw_alt(Raw_empty,Raw_char('a')))),Raw_char('c'))),Raw_char('b')),"accb")]

(* FIXED. bugs when I did not construct the CDN table yet *)
let cdn_empty: (raw_regex*string) list =
  [(Raw_quant(Plus,Raw_con(Raw_empty,Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_quant(Plus,Raw_char('c'))))))),"bbaaaacaabaabbbaaacacaacbccbacaacaaaaaabccbbbbbcbcccacabbbaabccccacccabacccacbcbbaacbccbcaacb");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_char('c'))),"ababaaaacbcaababaaccaccacbbbccabbabbcacaa");
   (Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_char('b'))),"baabacaccabacabcaaacbbbabacaabbcbbcbaccccaacbacbbababcccbcccacbabababcabbaabacbbbbcaaa");
   (Raw_lookaround(Lookbehind,Raw_alt(Raw_lookaround(Lookahead,Raw_lookaround(NegLookbehind,Raw_alt(Raw_dot,Raw_empty))),Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_quant(Plus,Raw_empty))))))),"babbccbbabacccabbabccccabacaacacbacabcaaccabbabccaca")]

(* fails the assertion "expected a nullable plus" *)
(* FIXED, when we don't forget to build a CDN table when reconstructing the + groups *)
let nullable_expected: (raw_regex*string) list =
  [(Raw_capture(Raw_con(Raw_capture(Raw_quant(Plus,Raw_capture(Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_char('b'))))))),Raw_lookaround(Lookahead,Raw_lookaround(Lookbehind,Raw_dot)))),"caabcbbcbcbcbacacaacbaccabaabbabbcbbbccbbbccac");
   (Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_lookaround(NegLookahead,Raw_empty)))),"caccaacbaabaaabaaacccaacacbbcabbababbacabbabcacabbcaabcbcaaacbabbcccbbcbbbcabbcaccacbacbb");
   (Raw_con(Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_alt(Raw_capture(Raw_capture(Raw_lookaround(NegLookahead,Raw_char('c')))),Raw_alt(Raw_dot,Raw_quant(LazyPlus,Raw_capture(Raw_quant(LazyStar,Raw_lookaround(Lookbehind,Raw_char('b'))))))))))),Raw_con(Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_empty)),Raw_char('b'))),"ababcbcaccbbcbbacccbcbccbbabaaaabbbbbabacacbccbabcbbbbabaacabaccabb");
   (Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_capture(Raw_dot)))),"aaccbcbccccccbbbccccbcbaabbaccbcccaabbacacccabbccaabbcabb");
   (Raw_lookaround(Lookbehind,Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(NegLookbehind,Raw_con(Raw_char('a'),Raw_con(Raw_char('b'),Raw_char('b'))))))),"bcaaabccbaabccaaaaababbaaaaaaaaaacbabacabcbbcbbaaabcbbaccabccbcacacc")]
  
(* JS is stuck (timeout), but not our engine *)
let redos : (raw_regex*string) list =
  [(Raw_lookaround(Lookbehind,Raw_con(Raw_lookaround(NegLookahead,Raw_dot),Raw_quant(LazyPlus,Raw_capture(Raw_con(Raw_quant(Star,Raw_char('a')),Raw_con(Raw_alt(Raw_empty,Raw_dot),Raw_dot)))))),"cbabbccccbbcccaaaaaccabccbaabaabcaaacbca");
   (Raw_con(Raw_con(Raw_lookaround(Lookahead,Raw_capture(Raw_capture(Raw_dot))),Raw_capture(Raw_alt(Raw_lookaround(Lookahead,Raw_dot),Raw_alt(Raw_con(Raw_capture(Raw_quant(Star,Raw_char('c'))),Raw_capture(Raw_char('a'))),Raw_alt(Raw_capture(Raw_quant(Plus,Raw_quant(Plus,Raw_lookaround(Lookahead,Raw_empty)))),Raw_capture(Raw_alt(Raw_alt(Raw_lookaround(NegLookbehind,Raw_quant(LazyPlus,Raw_capture(Raw_alt(Raw_con(Raw_capture(Raw_dot),Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_capture(Raw_capture(Raw_alt(Raw_char('a'),Raw_quant(LazyPlus,Raw_con(Raw_lookaround(NegLookbehind,Raw_quant(LazyPlus,Raw_lookaround(Lookahead,Raw_empty))),Raw_quant(Plus,Raw_alt(Raw_dot,Raw_capture(Raw_capture(Raw_dot))))))))),Raw_empty)))),Raw_lookaround(Lookahead,Raw_char('c')))))),Raw_dot),Raw_dot))))))),Raw_con(Raw_alt(Raw_con(Raw_alt(Raw_capture(Raw_quant(LazyStar,Raw_con(Raw_capture(Raw_char('b')),Raw_alt(Raw_empty,Raw_char('c'))))),Raw_dot),Raw_dot),Raw_lookaround(NegLookbehind,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookbehind,Raw_empty)))),Raw_lookaround(NegLookahead,Raw_alt(Raw_alt(Raw_capture(Raw_capture(Raw_empty)),Raw_lookaround(Lookbehind,Raw_empty)),Raw_lookaround(Lookahead,Raw_empty))))),"ccaacababbbcccbbbabcbbbccaaabccacbcb");
   (Raw_capture(Raw_alt(Raw_alt(Raw_con(Raw_alt(Raw_lookaround(NegLookbehind,Raw_con(Raw_con(Raw_alt(Raw_quant(Star,Raw_lookaround(Lookahead,Raw_capture(Raw_lookaround(Lookahead,Raw_capture(Raw_lookaround(Lookbehind,Raw_capture(Raw_empty))))))),Raw_empty),Raw_alt(Raw_char('a'),Raw_capture(Raw_quant(LazyPlus,Raw_lookaround(Lookahead,Raw_con(Raw_con(Raw_capture(Raw_alt(Raw_quant(Plus,Raw_capture(Raw_capture(Raw_capture(Raw_char('a'))))),Raw_char('b'))),Raw_empty),Raw_lookaround(Lookbehind,Raw_con(Raw_con(Raw_alt(Raw_capture(Raw_lookaround(Lookbehind,Raw_capture(Raw_con(Raw_lookaround(NegLookbehind,Raw_dot),Raw_empty)))),Raw_quant(LazyPlus,Raw_con(Raw_dot,Raw_quant(Star,Raw_alt(Raw_con(Raw_lookaround(NegLookahead,Raw_lookaround(Lookbehind,Raw_char('a'))),Raw_capture(Raw_dot)),Raw_capture(Raw_con(Raw_lookaround(NegLookahead,Raw_quant(LazyStar,Raw_empty)),Raw_capture(Raw_char('c'))))))))),Raw_char('b')),Raw_empty)))))))),Raw_lookaround(NegLookahead,Raw_con(Raw_capture(Raw_char('a')),Raw_con(Raw_empty,Raw_lookaround(Lookahead,Raw_quant(Star,Raw_char('c')))))))),Raw_alt(Raw_con(Raw_con(Raw_alt(Raw_lookaround(Lookbehind,Raw_capture(Raw_lookaround(Lookbehind,Raw_dot))),Raw_con(Raw_con(Raw_capture(Raw_lookaround(NegLookbehind,Raw_char('b'))),Raw_quant(LazyPlus,Raw_quant(Star,Raw_quant(LazyStar,Raw_alt(Raw_capture(Raw_capture(Raw_capture(Raw_capture(Raw_dot)))),Raw_dot))))),Raw_capture(Raw_capture(Raw_char('c'))))),Raw_con(Raw_lookaround(NegLookahead,Raw_quant(Plus,Raw_capture(Raw_char('b')))),Raw_capture(Raw_alt(Raw_con(Raw_dot,Raw_quant(Star,Raw_char('b'))),Raw_quant(Plus,Raw_lookaround(Lookbehind,Raw_lookaround(NegLookbehind,Raw_con(Raw_lookaround(NegLookahead,Raw_quant(Plus,Raw_char('c'))),Raw_dot)))))))),Raw_quant(Plus,Raw_empty)),Raw_quant(Plus,Raw_empty))),Raw_lookaround(Lookahead,Raw_capture(Raw_lookaround(NegLookbehind,Raw_empty)))),Raw_char('b')),Raw_char('b'))),"acccbcabbacbccbccbbcbbaa");
   (Raw_con(Raw_alt(Raw_char('a'),Raw_con(Raw_quant(LazyStar,Raw_capture(Raw_quant(Star,Raw_alt(Raw_dot,Raw_quant(Plus,Raw_empty))))),Raw_alt(Raw_char('b'),Raw_lookaround(NegLookahead,Raw_empty)))),Raw_con(Raw_lookaround(NegLookahead,Raw_capture(Raw_dot)),Raw_capture(Raw_capture(Raw_capture(Raw_dot))))),"cabbcaacbaccccababcbcccbababacbcccabbaaacacacbcacccaaacbbccabaabbaacbcbcacaaacabaacaaaa")]
  
(* re-checking a list of previous bugs *)
let replay_bugs (l:(raw_regex*string) list) =
  List.iter (fun (raw,str) -> ignore(compare_engines raw str)) l

(* just checking that our engine is not stuck on the REDOS regexes *)
let replay_stuck (l:(raw_regex*string) list) =
  List.iter (fun (raw,str) -> ignore(full_match raw str)) l

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
  replay_stuck(redos);
  Printf.printf "\027[32mTests passed\027[0m\n"


(* the example I put in the written description of the algorithm *)
let paper_example () =
  let lookahead_example = Raw_con(Raw_char('a'),Raw_capture(Raw_dot)) in
  let left_branch_example = Raw_con(Raw_quant(Star,Raw_char('a')),Raw_capture(Raw_char('b'))) in
  let right_branch_example = Raw_con(Raw_quant(Star,Raw_con(Raw_char('a'),Raw_lookaround(Lookahead,lookahead_example))),Raw_capture(Raw_dot)) in
  let reg_example : raw_regex = Raw_alt(right_branch_example,left_branch_example) in
  let str_example = "aaab" in
  full_match reg_example str_example
  
  
let main =
  (* let bug = List.nth nullable_expected 2 in *)
  (* ignore (get_linear_result ~verbose:true ~debug:true (fst bug) (snd bug)) *)
  (* ignore(get_js_result ~verbose:true ~debug:true (fst bug) (snd bug)); *)
  (* ignore(compare_engines (fst bug) (snd bug)) *)
  
  tests()
  (* fuzzer() *)
  (* run_benchmark(cdn_plus); *)
  (* experimental_benchmark() *)

  
    
