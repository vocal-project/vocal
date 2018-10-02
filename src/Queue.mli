
(*@ use Seq *)

type 'a t
(*@ ephemeral *)
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
      modifies q.view
      ensures  q.view == snoc (old q.view) x *)

val push : 'a -> 'a t -> unit
(** [push] is a synonym for [add]. *)
(*@ push x q
      equivalent "add x q" *)

val take : 'a t -> 'a
(** [take q] removes and returns the first element in queue [q],
   or raises [Empty] if the queue is empty. *)
(*@ r = take q
      raises Empty -> length q.view = 0
      modifies q.view
      ensures  old q.view == cons r q.view *)

val pop : 'a t -> 'a
(** [pop] is a synonym for [take]. *)
(*@ r = pop q
      equivalent "take q" *)

val peek : 'a t -> 'a
(** [peek q] returns the first element in queue [q], without removing
   it from the queue, or raises [Empty] if the queue is empty. *)
(*@ r = peek q
      raises  Empty -> length q.view = 0
      ensures length q.view > 0 (* ??? *)
      ensures r = q.view[0] *)

val top : 'a t -> 'a
(** [top] is a synonym for [peek]. *)
(*@ r = top q
      equivalent "peek q" *)

val clear : 'a t -> unit
(** Discard all elements from a queue. *)
(*@ clear q
      modifies q.view
      ensures  q.view == empty *)

val copy : 'a t -> 'a t
(** Return a copy of the given queue. *)
(*@ r = copy q
       ensures r.view == q.view *)

val is_empty : 'a t -> bool
(** Return [true] if the given queue is empty, [false] otherwise. *)
(*@ r = is_empty q
      ensures r <-> length q.view = 0 *)

val length : 'a t -> int
(** Return the number of elements in a queue. *)
(*@ r = length q
      ensures r = length q.view *)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** [fold f accu q] is equivalent to [List.fold_left f accu l],
   where [l] is the list of [q]'s elements. The queue remains
   unchanged. *)
(*@ r = fold f acc q
      (* assumes pure f *)
      ensures r = fold_left f acc q.view *)

val transfer : 'a t -> 'a t -> unit
(** [transfer q1 q2] adds all of [q1]'s elements at the end of
   the queue [q2], then clears [q1]. It is equivalent to the
   sequence [iter (fun x -> add x q2) q1; clear q1], but runs
   in constant time. *)
(*@ transfer q1 q2
      modifies q1.view, q2.view
      ensures  q1.view == empty
      ensures  q2.view == old q2.view ++ old q1.view *)

(*
val elements: 'a t -> 'a list
(*@ r = elements q
      ensures r == q.view *)
*)

val iter : ('a -> unit) -> 'a t -> unit
(** [iter f q] applies [f] in turn to all elements of [q],
   from the least recently entered to the most recently entered.
   The queue itself is unchanged. *)
(* @ iter f q
      equivalent "List.iter f (elements q)" *)

