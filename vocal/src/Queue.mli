(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2020 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

(** First-in first-out queues.

    This module implements queues (FIFOs), with in-place modification, following
    OCaml's implementation.

    Missing functions:
    - [copy]
    - [length]
    - [fold]
    - [iter]
*)

(*@ open Seq *)

type 'a t
(** The type of queues containing elements of type ['a]. *)
(*@ mutable model view: 'a seq *)


exception Empty
(** Raised when {!Queue.take} or {!Queue.peek} is applied to an empty queue. *)


val create : unit -> 'a t
(** Return a new queue, initially empty. *)
(*@ q = create ()
      ensures q.view == empty *)

val add : 'a -> 'a t -> unit
(** [add x q] adds the element [x] at the end of the queue [q]. *)
(*@ add x q
      modifies q
      ensures  q.view == Seq.snoc (old q.view) x *)

val push : 'a -> 'a t -> unit
(** [push] is a synonym for [add]. *)
(*@ push x q
      modifies q
      ensures  q.view == Seq.snoc (old q.view) x *)

val take : 'a t -> 'a
(** [take q] removes and returns the first element in queue [q],
   or raises {!Empty} if the queue is empty. *)
(*@ r = take q
      modifies q
      ensures  old q.view == Seq.cons r q.view
      ensures  old q.view == empty -> q.view == empty
      raises Empty -> (old q.view) == empty *)

val take_opt : 'a t -> 'a option
(** [take_opt q] removes and returns the first element in queue [q],
    or returns [None] if the queue is empty. 4.08 *)
(*@ r = take_opt q
      modifies q
      ensures  q.view == if old q.view == empty then old q.view
                                                else (old q.view)[1 ..]
      ensures  r = if old q.view == empty then None
                                          else Some ((old q.view)[0]) *)

val pop : 'a t -> 'a
(** [pop] is a synonym for [take]. *)
(*@ r = pop q
      modifies q
      ensures  old q.view == Seq.cons r q.view
      ensures  old q.view == empty -> q.view == empty
      raises   Empty -> (old q.view) == empty *)

val peek : 'a t -> 'a
(** [peek q] returns the first element in queue [q], without removing
    it from the queue, or raises {!Empty} if the queue is empty. *)
(*@ r = peek q
      ensures r = q.view[0]
      raises  Empty -> q.view == empty *)

val peek_opt : 'a t -> 'a option
(** [peek_opt q] returns the first element in queue [q], without removing
    it from the queue, or returns [None] if the queue is empty. *)
(*@ r = peek_opt q
      ensures r = if old q.view == empty then None else Some  q.view[0] *)

val top : 'a t -> 'a
(** [top] is a synonym for [peek]. *)
(*@ r = top q
      ensures r = q.view[0]
      raises  Empty -> q.view == empty *)

val clear : 'a t -> unit
(** Discard all elements from a queue. *)
(*@ clear q
      ensures q.view = empty *)

val is_empty : 'a t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)
(*@ b = is_empty q
      ensures b <-> q.view = empty *)

val transfer : 'a t -> 'a t -> unit
(** [transfer q1 q2] adds all of [q1]'s elements at the end of
   the queue [q2], then clears [q1]. It is equivalent to the
   sequence [iter (fun x -> add x q2) q1; clear q1], but runs
   in constant time. *)
(*@ transfer q1 q2
      modifies q1.view, q2.view
      ensures  q1.view == empty
      ensures  q2.view == old q2.view ++ old q1.view *)
