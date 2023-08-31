(** Representing Classes of Characters *)
(* including character ranges and negated classes *)

(* NOTE: OCaml chars go from 0 to 255 *)
(* While JS chars go to 65535 *)
(* Here we only deal with the first 256 chars *)

let min_char : char = char_of_int 0
let max_char : char = char_of_int 255

let prev_char (c:char) = char_of_int ((int_of_char c) -1)
let next_char (c:char) = char_of_int ((int_of_char c) +1)
let char_max (c1:char) (c2:char) = if c1 > c2 then c1 else c2

type char_expectation =
  | All                         (* expects any character *)
  | Single of char              (* a particular character *)
  | Ranges of (char * char) list (* several ranges of characters *)
(* In the case of Ranges, we expect the list to be ordered *)
(* Both chars defined the bounds of the range, and are both included *)

(* Note: I could express eveerything in terms of range *)
(* But I feel like the bytecode would be more readable with these 2 special cases *)
            
(** * Usual Ranges - stopped at 255 *)
(* \d *)
let digit : (char * char) list =
  [(char_of_int 48,char_of_int 57)]

(* \D *)
let nondigit : (char * char) list =
  [(char_of_int 0,char_of_int 47);(char_of_int 58,char_of_int 255)]

(* \w *)
let word : (char * char) list =
  [(char_of_int 48,char_of_int 57);
   (char_of_int 65,char_of_int 90);
   (char_of_int 95,char_of_int 95);
   (char_of_int 97,char_of_int 122)]

(* \W *)
let nonword : (char * char) list =
  [(char_of_int 0,char_of_int 47);
   (char_of_int 58,char_of_int 64);
   (char_of_int 91,char_of_int 94);
   (char_of_int 96,char_of_int 96);
   (char_of_int 123,char_of_int 255)]

(* \s *)
let space : (char * char) list =
  [(char_of_int 9,char_of_int 13);
   (char_of_int 32,char_of_int 32);
   (char_of_int 160,char_of_int 160)]
    (* for JS, also missing 5760, 8192-8202, 8232-8233, 8239, 8287, 12288, 65279 *)

(* \S *)
let nonspace : (char * char) list =
  [(char_of_int 0,char_of_int 8);
   (char_of_int 14,char_of_int 31);
   (char_of_int 33,char_of_int 159);
   (char_of_int 161,char_of_int 255)]
  

(** * Character Acceptance  *)

let rec is_in_range (c:char) (l:(char*char) list) : bool =
  match l with
  | [] -> false
  | (ch1,ch2)::l' ->
     if (c < ch1) then false    (* the list is assumed to be ordered *)
     else if (c >= ch1 && c <= ch2) then true
     else is_in_range c l'
             
(* is a read character accepted by an expectation *)
let is_accepted (read:char option) (ce:char_expectation): bool =
  match read,ce with
  | None, _ -> failwith "expected a character when consuming blocked thread"
  | _, All -> true
  | Some r, Single e -> r = e
  | Some r, Ranges l -> is_in_range r l


(** * Range Construction  *)


(* assumes that the list is ordered (according to the first element of the pairs *)
(* and that current has the smallest first element *)
(* assumes that pairs are correctly set (the first element is smaller) *)
let rec build_range (current:char*char) (next:(char*char) list) : (char*char) list =
  let (cstart,cend) = current in
  match next with
  | [] -> [current]
  | (nstart,nend)::next' ->
     (* we know nstart >= ctsart *)
     if (nstart > next_char cend) then
       (* disjoint ranges *)
       current::(build_range (nstart,nend) next')
     else
       (* extend from the end *)
       build_range (cstart, char_max cend nend) next'


type char_class_elt =
  | CChar of char
  | CRange of char * char

(* this is the contents between [] or [^] *)
(* it may be out of order *)
type char_class = char_class_elt list

let elt_to_range (e:char_class_elt) : (char*char) =
  match e with
  | CChar e -> (e,e)
  | CRange (c1,c2) ->
     assert (c1 <= c2);
     (c1, c2)
                
let class_to_range (c:char_class) : (char*char) list =
  let lranges = List.map elt_to_range c in
  let lsort = List.sort (fun (start1,e1) (start2,e2) -> if start1 > start2 then 1 else -1) lranges in
  match lsort with
  | [] -> []
  | head::tail -> build_range head tail

(** * Range Negation  *)

(* computes the ordered negation of an ordered list of ranges *)
let rec range_negation (l:(char*char) list) (min:char): (char*char) list =
  match l with
  | [] -> [(min,max_char)]
  | (r1,r2)::l' ->
     if (min < r1) then
       (min,prev_char r1)::(range_negation l' (next_char r2))
     else
       range_negation l' (next_char r2)

let range_neg (l:(char*char) list) : (char*char) list =
  range_negation l min_char

(* negates a char expectation *)
let negation (ce:char_expectation) : char_expectation =
  match ce with
  | All -> Ranges []
  | Single x -> Ranges (range_neg [(x,x)])
  | Ranges l -> Ranges (range_neg l)


(** * Pretty Printing  *)

let rec ranges_to_string (l:(char*char) list) : string =
  match l with
  | [] -> ""
  | (cstart,cend)::[] -> "("^String.make 1 cstart^","^String.make 1 cend^")"
  | (cstart,cend)::next -> "("^String.make 1 cstart^","^String.make 1 cend^");"^
                             ranges_to_string next
     
              
let expectation_to_string (ce:char_expectation) : string =
  match ce with
  | All -> "All"
  | Single x -> "Single " ^ String.make 1 x
  | Ranges l -> "Ranges [" ^ ranges_to_string l ^ "]"
