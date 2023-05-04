
open Oracle

let oracle_tests () =
  let o = create_oracle 4 5 in
  assert (Array.length o = 4);
  assert (Array.length (o.(0)) = 5);
  assert (get_oracle o 1 4 = false);
  set_oracle o 1 4;
  assert (get_oracle o 1 4 = true);
  assert (get_oracle o 1 3 = false);
  ()
   
let main =
  oracle_tests();
  Printf.printf "Tests passed\n"
