(** * Complexity Experiments  *)
(* Trying to validate that the complexity of our algorithm is linear in both size of the string and bytecode *)
(* While the JS backtracking engine isn't *)

(* We could also be linear in the size of the regex, but I believe that our compilation can be non-linear because of cleaning capture registers and lookaround memory (same issue in Experimental) *)
(* Also our compilation algo uses list concatenation and is probably quadratic *)

open Regex
open Tojs
open Toexp
open Tore2
open Torust
open Todotnet
open Interpreter
open Sys
open Unix
open Gc

let exp_dir = "exps/"

module Interpreter = Interpreter(Regs.List_Regs)
   
(** * A Benchmark Framework  *)
(* Regexes and strings parameterized with a size *)
type reg_param = int -> raw_regex
type str_param = int -> string

(* either we make the regex size vary or the string size *)
(* last string is a name *)
(* first int is the minimum size *)
(* second size is the maximum JS size *)
(* third int is the maximum OCaml size *)
type benchmark =
   | RegSize of (reg_param * string * int * int * int * string) 
   | StrSize of (raw_regex * str_param * int * int * int * string)

(* also measures compilation time... *)
let get_time_ocaml (r:raw_regex) (str:string) : float =
  Gc.full_major();               (* triggering the GC *)
  let tstart = Unix.gettimeofday() in
  ignore(Interpreter.get_linear_result r str);
  let tend = Unix.gettimeofday() in
  tend -. tstart

(* running benchmarks with the JaneStreet Core_bench library *)
let prepare_core_benchmark (b:benchmark) =
  let open Core in
  let open Core_bench in
  match b with
  | RegSize (rp, str, min, max_js, max_ocaml, name) ->
     let inputs = Array.init (max_ocaml-min+1) ~f:rp in
     let matcher_fn i =
       let r = inputs.(i) in
       (* todo live update i and try with a sleep *)
       Staged.stage (fun () -> ignore(Interpreter.get_linear_result r str)) in
     (inputs, matcher_fn, name)
  | _ -> failwith "TODO"
  
(* writes to a csv file and plots the result *)
let run_benchmark (b:benchmark) : unit =
  match b with
  | RegSize (rp, str, min, max_js, max_ocaml, name) -> 
     let oc = open_out (exp_dir^name^"_ocaml.csv") in
     for i = min to max_ocaml do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let reg = rp i in
       let tocaml = get_time_ocaml reg str in
       Printf.fprintf oc "%d,%f\n%!" i tocaml;
     done;
     close_out oc;
     Printf.printf "      \r%!";
     Unix.sleep 1;
     let oc = open_out (exp_dir^name^"_js.csv") in
     for i = min to max_js do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let reg = rp i in
       let tjs = get_time_js reg str in
       Printf.fprintf oc "%d,%s\n%!" i tjs;
     done;
     close_out oc;
     (* plotting the results *)
     let command = "python3.7 plot_exps.py " ^ name ^ " RegexSize " ^ " &" in
     ignore(string_of_command command)
  | StrSize (reg, strp, min, max_js, max_ocaml, name) ->
     let oc = open_out (exp_dir^name^"_ocaml.csv") in
     for i = min to max_ocaml do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let str = strp i in
       let tocaml = get_time_ocaml reg str in
       Printf.fprintf oc "%d,%f\n%!" i tocaml;
     done;
     close_out oc;
     Printf.printf "      \r%!";
     Unix.sleep 1;
     let oc = open_out (exp_dir^name^"_js.csv") in
     for i = min to max_js do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let str = strp i in
       let tjs = get_time_js reg str in
       Printf.fprintf oc "%d,%s\n%!" i tjs;
     done;
     close_out oc;
     (* plotting the results *)
     let command = "python3.7 plot_exps.py " ^ name ^ " StringSize " ^ " &" in
     ignore(string_of_command command)
     

(** * Simple Nested Plus  *)
(* I use two characters because Irregexp has been optimized when there is a single character in the repetition *)
     (* (aa+ )+ *)
let nested_plus_reg: raw_regex =
  Raw_con(Raw_quant(Plus,Raw_quant(Plus,Raw_con(raw_char('a'),raw_char('a')))),raw_char('b'))

let odd_a : str_param = fun str_size ->
  String.make (2*str_size +1) 'a'

let nested_plus : benchmark =
  StrSize (nested_plus_reg, odd_a, 0, 23, 5000, "NestedPlus")
   
(** * Lookahead in a Star  *)
   
(* (a(?=a*b))* *)
let lookahead_star_reg: raw_regex = 
  Raw_quant(Star,Raw_con(raw_char('a'),Raw_lookaround(Lookahead,Raw_con(Raw_quant(Star,raw_char('a')),raw_char('b'))))) 

let a_repeat_b : str_param = fun str_size -> 
  (String.make str_size 'a') ^ (String.make 1 'b')
  
let lookahead_star : benchmark =
  StrSize (lookahead_star_reg, a_repeat_b, 0, 3000, 3000, "Lookahead_Star")
  
(** * Nested Lookaheads  *)

(* a(?=a) -> a(?=a(?=a)) -> a(?=a(?=a(?=a))) -> ... *)
let rec nested_lookahead_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> raw_char('a')
  | _ -> Raw_con(raw_char('a'),Raw_lookaround(Lookahead,nested_lookahead_reg(reg_size-1)))

let nested_string : string =
  String.make 600 'a'

let lookahead_nested : benchmark =
  RegSize (nested_lookahead_reg, nested_string, 0, 500, 500, "Lookahead_Nested")


(** * Double Star Explosion  *)
  (* (a* )*b explodes on aaaaa, so let's try ((a(?=.))* )*b to add a lookahead *)
let explosion_reg : raw_regex =
  Raw_con(Raw_quant(Star,Raw_quant(Star,Raw_con(raw_char('a'),Raw_lookaround(Lookahead,raw_dot)))),raw_char('b'))

let a_repeat : str_param = fun str_size ->
  String.make str_size 'a'

let double_star_explosion : benchmark =
  StrSize (explosion_reg, a_repeat, 0, 34, 300, "DoubleStarExplosion")


(** * Possibly Quadratic  *)
(* because of the way we clear capture registers (same in Experimental) *)
(* our bytecode might be quadratic in the regex size, and thus execution time could be as well *)
  

let rec quadratic_bytecode : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> raw_dot
  | _ -> Raw_quant(Star, Raw_capture (quadratic_bytecode (reg_size-1)))

let quadratic_string : string =
  String.make 100 'b'

let possibly_quadratic : benchmark =
  RegSize (quadratic_bytecode, quadratic_string, 0, 200, 200, "PossiblyQuadratic")


(** * Checking that the V8 Experimental engine is also quadratic  *)
let experimental_benchmark () =
  let oc = open_out (exp_dir^"experimental_quadratic.csv") in
  for i = 0 to 200 do
    let reg = quadratic_bytecode i in
    let texp = get_time_experimental reg quadratic_string in
    Printf.fprintf oc "%d,%s\n" i texp;
  done;
  close_out oc;
  (* plotting the results *)
  let command = "python3.7 plot_single.py experimental_quadratic V8Experimental &" in
  ignore(string_of_command command)


  
(** * Plus Quadratic  *)
(* because Plus gets compiled to a concatenation with a star, we can get quadratic bytecode length *)
(* Note that after 5 nested Plus, Experimental simply rejects the regex *)

let rec quadratic_plus_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> raw_dot
  | _ -> Raw_quant(Plus,quadratic_plus_reg (reg_size - 1))

let quadratic_plus_str : string =
  String.make 999 'a'

let quadratic_plus : benchmark =
  RegSize (quadratic_plus_reg, quadratic_plus_str, 0, 500, 500, "QuadraticPlusNN")

  
(** * Nested Nullables  *)
  
let rec nested_nullable_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> Raw_quant(Star,Raw_alt(Raw_empty,raw_dot))
  | _ -> Raw_quant(Star,Raw_alt(nested_nullable_reg (reg_size - 1),raw_dot))

let nested_null_string : string =
  String.make 999 'a'           (* <1000, not JS compiled *)

let nested_nullable : benchmark =
  RegSize (nested_nullable_reg, nested_null_string, 0, 400, 400, "NestedNullables")

(** * CIN Plus   *)
(* Context independent nullable nested plus *)
  
let rec cin_plus_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> Raw_alt(raw_dot,Raw_empty)
  | _ -> Raw_quant(Plus,cin_plus_reg (reg_size - 1))

let cin_plus_str : string =
  String.make 999 'a'

let cin_plus : benchmark =
  RegSize (cin_plus_reg, cin_plus_str, 0, 25, 400, "CINPlus")

(** * CDN Plus   *)
(* Context dependent nullable nested plus *)
  
let rec cdn_plus_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> Raw_alt(raw_dot,Raw_lookaround(Lookahead,raw_dot))
  | _ -> Raw_quant(Plus,cdn_plus_reg (reg_size - 1))

let cdn_plus_str : string =
  String.make 999 'a'

let cdn_plus : benchmark =
  RegSize (cdn_plus_reg, cdn_plus_str, 0, 400, 400, "CDNPlus")

(** * Many Forks  *)

let int_to_alphabet (idx:int) : char =
  char_of_int(int_of_char ('a') + idx mod 26)
  
let many_forks_reg : reg_param = 
  let rec many_forks_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> raw_char('a')
  | _ -> Raw_capture(Raw_alt(many_forks_reg (reg_size-1),raw_char(int_to_alphabet reg_size)))
  in
  fun reg_size ->         
  Raw_quant(Star,many_forks_reg reg_size)

let many_forks_str = String.make 9 'a'

let many_forks : benchmark =
  RegSize(many_forks_reg, many_forks_str, 0, 2000, 2000, "ManyForks")

let many_forks_experimental_benchmark () =
  let oc = open_out (exp_dir^"experimental_many_forks.csv") in
  for i = 0 to 1000 do
    Printf.printf " %s\r%!" (string_of_int i); (* live update *)
    let reg = many_forks_reg i in
    let texp = get_time_experimental reg many_forks_str in
    Printf.fprintf oc "%d,%s\n%!" i texp;
  done;
  close_out oc;
  (* plotting the results *)
  let command = "python3.7 plot_single.py experimental_many_forks V8Experimental &" in
  ignore(string_of_command command)
    
let many_forks_re2_benchmark () =
  let oc = open_out (exp_dir^"re2_many_forks.csv") in
  for i = 0 to 3000 do
    Printf.printf " %s\r%!" (string_of_int i); (* live update *)
    let reg = many_forks_reg i in
    let texp = get_time_re2 reg many_forks_str in
    Printf.fprintf oc "%d,%f\n%!" i texp;
  done;
  close_out oc;
  (* plotting the results *)
  let command = "python3.7 plot_single.py re2_many_forks RE2 &" in
  ignore(string_of_command command)

let many_forks_rust_benchmark () =
  let oc = open_out (exp_dir^"rust_many_forks.csv") in
  for i = 0 to 82 do
    Printf.printf " %s\r%!" (string_of_int i); (* live update *)
    let reg = many_forks_reg i in
    let texp = get_time_rust reg many_forks_str in
    Printf.fprintf oc "%d,%s\n%!" i texp;
  done;
  close_out oc;
  (* plotting the results *)
  let command = "python3.7 plot_single.py rust_many_forks Rust &" in
  ignore(string_of_command command)


(** * .NET non-linear  benchmark  *)
let dotnet_benchmark () =
  let double_star = Raw_con (Raw_quant(Star,Raw_capture(Raw_quant(Star,raw_char('a')))),raw_char('b')) in
  let oc = open_out (exp_dir^"double_star_dotnet.csv") in
  for i = 0 to 26 do
    Printf.printf " %s\r%!" (string_of_int i); (* live update *)
    let str = a_repeat i in
    let tdn = get_time_dotnet false double_star str in
    Printf.fprintf oc "%d,%s\n%!" i tdn;
  done;
  close_out oc;
  (* plotting the results *)
  let command = "python3.7 plot_single.py double_star_dotnet DotNet &" in
  ignore(string_of_command command)
