(** * Thread register implementations  *)


open Array
open Map


module IntMap = Map.Make(struct type t = int let compare = compare end)


module type REGS =
  sig
    type regs
    val init_regs: int -> regs
    val set_reg: regs -> int -> int -> regs
    val clear_reg: regs -> int -> regs
    (* we might remove get_regs and always convert to an array first for filtering *)
    val get_reg: regs -> int -> int option
    val copy: regs -> regs
    val to_array: regs -> int Array.t
    val to_string: regs -> string (* for debugging purposes *)
  end

   
module Array_Regs =
  struct
    (* all the values stored are positive *)
    (* -1 represents the absence of a value *)
    type regs = int Array.t

    let init_regs (size:int) : regs =
      Array.make ((2*size)+2) (-1)

    let set_reg (regs:regs) (k:int) (v:int) : regs =
      regs.(k) <- v;
      regs

    let clear_reg (regs:regs) (k:int) : regs =
      regs.(k) <- -1;
      regs

    let get_reg (regs:regs) (k:int) : int option =
      let v = regs.(k) in
      if (v < 0) then None else Some v

    let copy (regs:regs) : regs =
      Array.copy regs

    let to_array (regs:regs) : int Array.t =
      regs

    let to_string (regs:regs) : string =
      let s = ref "" in
      for c = 0 to (Array.length regs)-1 do
        s := !s ^ string_of_int c ^ ": " ^ string_of_int (regs.(c)) ^ " | "
      done;
      !s
  end

module List_Regs =
  struct
    type regs = ((int*int) list * int) (* we also remember the size (nb of registers) *)

    let init_regs (size:int) : regs =
     ([], size)

    let set_reg (regs:regs) (k:int) (v:int) : regs =
      ((k,v)::(fst regs), snd regs)

    let clear_reg (regs:regs) (k:int) : regs =
      ((k,-1)::(fst regs), snd regs)

    let rec get_reg (regs:regs) (k:int) : int option =
      match fst regs with
      | [] -> None
      | (kl,vl)::regs' ->
         if (kl = k) then Some vl else
           get_reg (regs',snd regs) k

    let copy (regs:regs) : regs =
      regs

    let to_array (regs:regs) : int Array.t =
      let a = Array.make (2*(snd regs) + 2) (-1) in
      let rec fill_array (l:(int*int) list) : unit =
        match l with
        | [] -> ()
        | (k,v)::l' ->
           (* only setting reg values that haven't been set yet *)
           if (a.(k) = -1) then a.(k) <- v;
           fill_array l' in
      fill_array (fst regs);
      a

    let rec to_string (regs:regs) : string =
      match (fst regs) with
      | [] -> ""
      | (k,v)::l' ->
         "(" ^ string_of_int k ^ "," ^ string_of_int v ^ ")::" ^ to_string (l', snd regs)
  end


module Map_Regs =
  struct
    type regs = (int IntMap.t * int) (* we also remember the size (nb of registers) *)

    let init_regs (size:int) : regs =
      (IntMap.empty, size)

    let set_reg (regs:regs) (k:int) (v:int) : regs =
      (IntMap.add k v (fst regs), snd regs)

    let clear_reg (regs:regs) (k:int) : regs =
      (IntMap.remove k (fst regs), snd regs)

    let get_reg (regs:regs) (k:int) : int option =
      IntMap.find_opt k (fst regs)

    let copy (regs:regs) : regs =
      regs

    let to_array (regs:regs) : int Array.t =
      let a = Array.make (2*(snd regs) + 2) (-1) in
      for i = 0 to (Array.length a)-1 do
        match get_reg regs i with
        | Some v -> a.(i) <- v
        | None -> ()
      done;
      a

    let string_of_opt (i:int option) : string =
      match i with
      | None -> "-1"
      | Some v -> string_of_int v
      
    let to_string (regs:regs) : string =
      let s = ref "" in
      for c = 0 to (2*(snd regs) + 1) do
        s := !s ^ string_of_int c ^ ": " ^ string_of_opt (get_reg regs c) ^ " | "
      done;
      !s

  end
        
           
