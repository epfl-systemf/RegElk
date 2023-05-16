(** * Complexity Experiments  *)
(* Trying to validate that the complexity of our algorithm is linear in both size of the string and bytecode *)
(* While the JS backtracking engine isn't *)

(* We could also be linear in the size of the regex, but I believe that our compilation can be non-linear because of cleaning capture registers and lookaround memory (same issue in Experimental) *)
(* Also our compilation algo uses list concatenation and is probably quadratic *)

open Regex
open Tojs
open Linear
open Sys

(** * Lookahead in a Star  *)
   
(* (a(?=a*b))* *)
let lookahead_star () : raw_regex =
  Raw_quant(Star,Raw_con(Raw_char('a'),Raw_lookaround(Lookahead,Raw_con(Raw_quant(Star,Raw_char('a')),Raw_char('b'))))) 

let a_repeat_b (str_size:int) : string =
  (String.make str_size 'a') ^ (String.make 1 'b')
  
let lookahead_star_js (str_size:int) : float =
  let regex = lookahead_star() in
  let str = a_repeat_b str_size in (* building arguments before measuring time *)
  let tstart = Sys.time() in
  ignore(get_js_result regex str);
  let tend = Sys.time() in
  tend -. tstart


let lookahead_star_linear (str_size:int) : float =
  let regex = lookahead_star() in
  let str = a_repeat_b str_size in
  let tstart = Sys.time() in
  ignore(get_linear_result regex str);
  let tend = Sys.time() in
  tend -. tstart

let max_test : int = 500

let lookahead_star_test (filename:string) : unit =
  let oc = open_out filename in
  for i = 0 to max_test do
    let tli = lookahead_star_linear i in
    let tjs = lookahead_star_js i in
    Printf.fprintf oc "%d,%f,%f\n" i (tli) (tjs)
  done;
  close_out oc;
  (* plotting the results *)
  let command = "python3 plot_exps.py " ^ filename in
  ignore(string_of_command command)
  
