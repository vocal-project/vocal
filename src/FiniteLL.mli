
(* Finite singly-linked lists

   These are lists of type SinglyLL.t, which are known to be Nil-terminated. *)

type 'a t
(*@ ephemeral *)
(*@ mutable model view: 'a seq *)

val nil: unit -> 'a t
(*@ l = nil()
       ensures l.view == empty *)

val is_nil: 'a t -> bool
(*@ b = is_nil l
       ensures b <-> length l.view = 0 *)

val length: 'a t -> int
(*@ n = length l
       ensures n = length l.view *)

val cons: 'a -> 'a t -> 'a t
(*@ r = cons x l
       ensures r.view == cons x l.view *)

val tail: 'a t -> 'a t
(*@ t = tail l
       requires length l.view > 0
       ensures  t.view = l.view[1 .. ] *)

val stable_mergesort: ('a -> 'a -> int) -> 'a t -> 'a t
(*@ r = stable_mergesort cmp l
       requires is_pre_order cmp
       modifies l
       ensures  permut r.view l.view
       ensures  is_sorted cmp r.view *)
