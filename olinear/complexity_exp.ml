(** * Complexity Experiments  *)
(* Trying to validate that the complexity of our algorithm is linear in both size of the string and bytecode *)
(* While the JS backtracking engine isn't *)

(* We could also be linear in the size of the regex, but I believe that our compilation can be non-linear because of cleaning capture registers and lookaround memory (same issue in Experimental) *)
(* Also our compilation algo uses list concatenation and is probably quadratic *)

open Regex
open Tojs
open Toexp
open Linear
open Sys
open Unix
open Gc
   
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


let get_time_ocaml (r:raw_regex) (str:string) : float =
  Gc.full_major();               (* triggering the GC *)
  let tstart = Unix.gettimeofday() in
  ignore(get_linear_result r str);
  let tend = Unix.gettimeofday() in
  tend -. tstart

(* writes to a csv file and plots the result *)
let run_benchmark (b:benchmark) : unit =
  match b with
  | RegSize (rp, str, min, max_js, max_ocaml, name) -> 
     let oc = open_out (name^"_ocaml.csv") in
     for i = min to max_ocaml do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let reg = rp i in
       let tocaml = get_time_ocaml reg str in
       Printf.fprintf oc "%d,%f\n" i tocaml;
     done;
     close_out oc;
     Printf.printf "      \r%!";
     Unix.sleep 1;
     let oc = open_out (name^"_js.csv") in
     for i = min to max_js do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let reg = rp i in
       let tjs = get_time_js reg str in
       Printf.fprintf oc "%d,%s\n" i tjs;
     done;
     close_out oc;
     (* plotting the results *)
     let command = "python3 plot_exps.py " ^ name ^ " RegexSize " ^ " &" in
     ignore(string_of_command command)
  | StrSize (reg, strp, min, max_js, max_ocaml, name) ->
     let oc = open_out (name^"_ocaml.csv") in
     for i = min to max_ocaml do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let str = strp i in
       let tocaml = get_time_ocaml reg str in
       Printf.fprintf oc "%d,%f\n" i tocaml;
     done;
     close_out oc;
     Printf.printf "      \r%!";
     Unix.sleep 1;
     let oc = open_out (name^"_js.csv") in
     for i = min to max_js do
       Printf.printf " %s\r%!" (string_of_int i); (* live update *)
       let str = strp i in
       let tjs = get_time_js reg str in
       Printf.fprintf oc "%d,%s\n" i tjs;
     done;
     close_out oc;
     (* plotting the results *)
     let command = "python3 plot_exps.py " ^ name ^ " StringSize " ^ " &" in
     ignore(string_of_command command)
     
  
   
(** * Lookahead in a Star  *)
   
(* (a(?=a*b))* *)
let lookahead_star_reg: raw_regex = 
  Raw_quant(Star,Raw_con(Raw_char('a'),Raw_lookaround(Lookahead,Raw_con(Raw_quant(Star,Raw_char('a')),Raw_char('b'))))) 

let a_repeat_b : str_param = fun str_size -> 
  (String.make str_size 'a') ^ (String.make 1 'b')

let lookahead_star : benchmark =
  StrSize (lookahead_star_reg, a_repeat_b, 0, 3000, 3000, "Lookahead_Star")
  
(** * Nested Lookaheads  *)

(* a(?=a) -> a(?=a(?=a)) -> a(?=a(?=a(?=a))) -> ... *)
let rec nested_lookahead_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> Raw_char('a')
  | _ -> Raw_con(Raw_char('a'),Raw_lookaround(Lookahead,nested_lookahead_reg(reg_size-1)))

let nested_string : string =
  String.make 600 'a'

let lookahead_nested : benchmark =
  RegSize (nested_lookahead_reg, nested_string, 0, 500, 500, "Lookahead_Nested")


(** * Double Star Explosion  *)
  (* (a* )*b explodes on aaaaa, so let's try ((a(?=.))* )*b to add a lookahead *)
let explosion_reg : raw_regex =
  Raw_con(Raw_quant(Star,Raw_quant(Star,Raw_con(Raw_char('a'),Raw_lookaround(Lookahead,Raw_dot)))),Raw_char('b'))

let a_repeat : str_param = fun str_size ->
  String.make str_size 'a'

let double_star_explosion : benchmark =
  StrSize (explosion_reg, a_repeat, 0, 34, 300, "DoubleStarExplosion")


(** * Possibly Quadratic  *)
(* because of the way we clear capture gristers (same in Experimental) *)
(* our bytecode might be quadratic in the regex size, and thus execution time could be as well *)
  

let rec quadratic_bytecode : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> Raw_dot
  | _ -> Raw_quant(Star, Raw_capture (quadratic_bytecode (reg_size-1)))

let quadratic_string : string =
  String.make 100 'b'

let possibly_quadratic : benchmark =
  RegSize (quadratic_bytecode, quadratic_string, 0, 200, 200, "PossiblyQuadratic")


(** * Checking that the V8 Experimental engine is also quadratic  *)
let experimental_benchmark () =
  let oc = open_out ("experimental_quadratic.csv") in
  for i = 0 to 200 do
    let reg = quadratic_bytecode i in
    let texp = get_time_experimental reg quadratic_string in
    Printf.fprintf oc "%d,%s\n" i texp;
  done;
  close_out oc;
  (* plotting the results *)
  let command = "python3 plot_single.py experimental_quadratic V8Experimental &" in
  ignore(string_of_command command)


  
(** * Plus Quadratic  *)
(* because Plus gets compiled to a concatenation with a star, we can get quadratic bytecode length *)
(* Note that after 5 nested Plus, Experimental simply rejects the regex *)

let rec quadratic_plus_reg : reg_param = fun reg_size ->
  match reg_size with
  | 0 -> Raw_dot
  | _ -> Raw_quant(Plus,quadratic_plus_reg (reg_size - 1))

let quadratic_plus_str : string =
  String.make 999 'a'

let quadratic_plus : benchmark =
  RegSize (quadratic_plus_reg, quadratic_plus_str, 0, 20, 20, "QuadraticPlus")

