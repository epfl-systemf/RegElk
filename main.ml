open Oracle
open Regex
open Bytecode
open Compiler
open Cdn
open Interpreter
open Tojs
open Charclasses
open Flags

(* Executing the OCaml linear engine on a regex and a string *)

let input_str = ref ""
let input_regex = ref ""
let str_set = ref false
let rgx_set = ref false
let compare_js = ref false 
   
(* fails if the regex is not correct *)
let parse_raw (str:string) : raw_regex =
  let r:raw_regex = Regex_parser.main Regex_lexer.token (Lexing.from_string str) in
  assert (regex_wf r);
  r

(* Appendix example *)
let appendix_regex : raw_regex =
  let behind = Raw_lookaround(Lookbehind,Raw_con(raw_char('c'),raw_star(Raw_capture(raw_char('a'))))) in
  let ahead = Raw_lookaround(Lookahead,Raw_con(raw_star(raw_char('a')),Raw_con(behind,raw_char('b')))) in
  Raw_con(Raw_capture(raw_char('c')),raw_star(Raw_con(raw_char('a'),ahead)))

let appendix_string : string = "caab"
                              
  
  
let main =

  (* let bug = (Raw_con(Raw_quant(LazyPlus,Raw_capture(Raw_quant(LazyStar,Raw_character(Dot)))),Raw_character(Char('a'))),"b-a") in
   * debug := true;
   * verbose := true;
   * ignore(get_linear_result (fst bug) (snd bug));
   * ignore(compare_engines (fst bug) (snd bug)); *)
  
  
  let speclist =
    [("-regex", Arg.Tuple [Arg.Set_string input_regex; Arg.Set rgx_set], "Regex");
     ("-string", Arg.Tuple [Arg.Set_string input_str; Arg.Set str_set], "String");
     ("-v", Arg.Set verbose, "Verbose Mode");
     ("-d", Arg.Set debug, "Debug Mode");
     ("-cmp", Arg.Set compare_js, "Comparison with the Node engine");
    ] in

  let usage = "./main.native [-regex \"(b)|.*\"] [-string \"abc\"] [-v] [-d] [-cmp]" in
  Arg.parse speclist (fun _ -> ()) usage;

  (* if no regex or string were provided, ask the user to input them *)
  if not !rgx_set then begin
      Printf.printf "\027[36mEnter your regex:\027[0m\n";
      input_regex := read_line ()
    end;
  
  let regex = parse_raw !input_regex in
  if !verbose then Printf.printf "\027[33mParsed Regex:\027[0m\n%s\n" (report_raw regex);
  
  if not !str_set then begin
      Printf.printf "\027[36mEnter your string:\027[0m\n";
      input_str := read_line ()
    end;
  
  if !compare_js then
    ignore (compare_engines regex !input_str)
  else
    Printf.printf "%s" (get_linear_result regex !input_str)



         (* TODO: make some benchmark binaries *)
  (* run_benchmark(many_forks) *)
  (* many_forks_rust_benchmark() *)

  (* TODO: make more JSCore benchmarks *)
  (* let open Core in
   *   let open Core_bench in
   *   let (array_args,matcher_fn,name) = prepare_core_benchmark quadratic_plus in
   *   Command_unix.run (Bench.make_command [
   *                    Bench.Test.create_indexed
   *                      ~name
   *                      ~args:(List.init (Array.length array_args) (fun i -> i))
   *                      matcher_fn ]) *)
