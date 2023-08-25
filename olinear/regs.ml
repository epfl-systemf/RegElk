(** * Thread register implementations  *)


open Array


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
  end

   
module Array_Regs =
  struct
    (* all the values stored are positive *)
    (* -1 represents the absence of a value *)
    type regs = int Array.t

    let init_regs (size:int) : regs =
      Array.make (size+1) (-1)

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
      let a = Array.make ((snd regs) + 1) (-1) in
      let rec fill_array (l:(int*int) list) : unit =
        match l with
        | [] -> ()
        | (k,v)::l' ->
           (* only setting reg values that haven't been set yet *)
           if (a.(k) = -1) then a.(k) <- v;
           fill_array l' in
      fill_array (fst regs);
      a
  end
