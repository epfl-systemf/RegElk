(** * Defining the Oracl type and operations  *)
(* the array in which we store for each position of the string if each lookaround can be matched or not *)

open Array

type oracle = bool Array.t Array.t 

let create_oracle (str_size:int) (look_nb:int): oracle =
  assert (str_size > 0);
  Array.make_matrix str_size look_nb false

(* we only allow setting to true, there's no reason to set an entry of the table back to false *)
let set_oracle (o:oracle) (cp:int) (lookid:int): unit =
  assert (cp < Array.length o);
  assert (lookid < Array.length o.(0));
  o.(cp).(lookid) <- true

let get_oracle (o:oracle) (cp:int) (lookid:int): bool =
  assert (cp < Array.length o);
  assert (lookid < Array.length o.(0));
  o.(cp).(lookid)
  
