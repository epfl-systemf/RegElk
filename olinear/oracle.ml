(** * Defining the Oracl type and operations  *)
(* In this version (only handling lookbehinds), the oracle is a small array *)
(* of the size of the number of lookbehinds in the regex O(r) *)
(* it gets reinitialized at each step in the string *)
(* it gets filled up from the deepest lookbehind to the shallowest one *)

open Array

type oracle = bool Array.t

let create_oracle (look_nb:int): oracle =
  Array.make (look_nb+1) false

(* we only allow setting to true, there's no reason to set an entry of the table back to false *)
let set_oracle (o:oracle) (lookid:int): unit =
  assert (lookid < Array.length o);
  o.(lookid) <- true

let get_oracle (o:oracle) (lookid:int): bool =
  assert (lookid < Array.length o);
  o.(lookid)

(** * Pretty-printing  *)
  
let print_bool (b:bool) : string =
  if b then "\027[32m✔\027[0m" else "\027[31m✘\027[0m"
  
let print_oracle (o:oracle) : string =
  let s = ref "\027[31mOracle:\027[0m\n" in
  for j = 0 to ((Array.length o) -1) do
    s := !s ^ print_bool (get_oracle o j);
  done;
  !s
