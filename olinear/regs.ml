(** * Thread register implementations  *)


open Array


module type REGS =
  sig
    type regs
    type key
    val init_regs: int -> regs
    val set_reg: regs -> key -> int -> regs
    val clear_reg: regs -> key -> regs
    val get_reg: regs -> key -> int option
    val copy: regs -> regs
  end

   
module Array_Regs =
  struct
    (* all the values stored are positive *)
    (* -1 represents the absence of a value *)
    type regs = int Array.t
    type key = int

    let init_regs (size:int) : regs =
      Array.make size (-1)

    let set_reg (regs:regs) (k:key) (v:int) : regs =
      regs.(k) <- v;
      regs

    let clear_reg (regs:regs) (k:key) : regs =
      regs.(k) <- -1;
      regs

    let get_reg (regs:regs) (k:key) : int option =
      let v = regs.(k) in
      if (v < 0) then None else Some v

    let copy (regs:regs) : regs =
      Array.copy regs
  end

module List_Regs =
  struct
    type regs = (int*int) list
    type key = int

    let init_regs (_:int) : regs =
      []

    let set_reg (regs:regs) (k:key) (v:int) : regs =
      (k,v)::regs

    let clear_reg (regs:regs) (k:key) : regs =
      (k,-1)::regs

    let rec get_reg (regs:regs) (k:key) : int option =
      match regs with
      | [] -> None
      | (kl,vl)::regs' ->
         if (kl = k) then vl else
           get_reg regs' k

    let copy (regs:regs) : regs =
      regs
  end
