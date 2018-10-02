(*@ use Set *)

type 'a t
(** polymorphic type of hash sets *)
(*@ ephemeral *)
(*@ mutable model dom: 'a set *)

val create : ?random:bool -> int -> 'a t
(*@ s = create ?random n
      ensures dom s = {} *)

val add : 'a t -> 'a -> unit
(*@ add s k
      modifies s
      ensures  dom s = old (dom s) `union` {k} *)

val mem : 'a t -> 'a -> bool
(*@ b = mem s k
      ensures b <-> mem k (dom s) *)

val remove : 'a t -> 'a -> unit
(*@ remove s k
      modifies s
      ensures  dom s = remove k (old dom s) *)

val length : 'a t -> int
(*@ n = length s
      ensures n = cardinal (dom s) *)
