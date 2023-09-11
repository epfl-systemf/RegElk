(* Stats about the regex corpora *)

open Parser
open Regex
   
let main =
  let filename = "corpus/npm-uniquePatterns.json" in
  let stats = init_stats() in
  let chan = open_in filename in
  try
    while true; do
      let line = input_line chan in
      let regex_str = String.sub line 13 (String.length line - 15) in
      Printf.printf "\n\027[36m%s\027[0m\n%!" regex_str; 
      let result = parse regex_str stats in
      Printf.printf "%s\n%!" (print_result result)
    done;
  with End_of_file ->
    close_in chan;
    Printf.printf ("\n%s\n") (print_stats stats)
    
