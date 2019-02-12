#use "4.ml"
#use "5.ml"
open Stream;;

exception Empty
exception Impossible_pattern of string

let impossible_pat x = raise (Impossible_pattern x)


module type QUEUE = sig
  type 'a queue

  val empty : 'a queue
  val is_empty : 'a queue -> bool

  val snoc : 'a queue -> 'a -> 'a queue
  val head : 'a queue -> 'a        (* raises Empty if queue is empty *)
  val tail : 'a queue -> 'a queue  (* raises Empty if queue is empty *)
end


(* A totally ordered type and its comparison functions *)
module type ORDERED = sig
  type t

  val eq : t -> t -> bool
  val lt : t -> t -> bool
  val leq : t -> t -> bool
end


module type HEAP = sig
  module Elem : ORDERED

  type heap

  val empty : heap
  val is_empty : heap -> bool

  val insert : Elem.t -> heap -> heap
  val merge : heap -> heap -> heap

  val find_min : heap -> Elem.t  (* raises Empty if heap is empty *)
  val delete_min : heap -> heap  (* raises Empty if heap is empty *)
end


(* ---------- Streams as found in chapter 4 ---------- *)



module BankersQueue : QUEUE = struct
  type 'a queue = int * 'a stream * int * 'a stream

  let empty = 0, lazy Nil, 0, lazy Nil
  let is_empty (lenf, _, _, _) = lenf = 0

  let check (lenf, f, lenr, r as q) =
    if lenr <= lenf then q
    else (lenf + lenr, f ++ reverse r, 0, lazy Nil)

  let snoc (lenf, f, lenr, r) x =
    check (lenf, f, lenr + 1, lazy (Cons (x, r)))

  let head = function
    | _, lazy Nil, _, _ -> raise Empty
    | _, lazy (Cons (x, _)), _, _ -> x

  let tail = function
    | _, lazy Nil, _, _ -> raise Empty
    | lenf, lazy (Cons (_, f')), lenr, r -> check (lenf - 1, f', lenr, r)
end


