(** Hash tables using OCaml's polymorphic hash functions.

    Hash tables are hashed association tables, with in-place modification. *)

(*@ use List *)
(*@ use Map  *)
(*@ use Set  *)

(** {6 Generic interface} *)

type ('a, 'b) t
(** The type of hash tables from type ['a] to type ['b]. *)
(*@ ephemeral *)
(*@ mutable model dom  : 'a set *)
(*@ mutable model data : 'a -> 'b list *)
(*@ invariant forall x. mem x dom <-> data x <> Nil *)

val create : ?random:bool -> int -> ('a, 'b) t
(*@ h = create ?random n
      ensures forall x. data h x = Nil
      ensures dom h = {} *)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(*@ add h k v
      modifies h
      ensures  data h k = Cons v (old (data h k))
      ensures  forall x. x <> k -> data h x = old (data h k)
      ensures  dom h = add k (old (dom h)) *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(*@ replace h k v
      modifies h
      ensures  data h k
             = Cons v (match old (data h k) with Nil -> Nil | Cons _ l -> l)
      ensures  forall x. x <> k -> data h x = old (data h k)
      ensures  dom h = add k (old (dom h)) *)

val mem : ('a, 'b) t -> 'a -> bool
(*@ b = mem h k
      ensures b <-> data h k <> Nil *)

val remove : ('a, 'b) t -> 'a -> unit
(*@ remove h k
      modifies h
      ensures  data h k = (match old (data h k) with Nil -> Nil | Cons _ l -> l)
      ensures  forall x. x <> k -> data h x = old (data h x)
      ensures  dom h = match old (data h k) with
                       | Cons _ Nil -> remove k (old (dom h))
                       | _          -> old (dom h) *)

val find : ('a, 'b) t -> 'a -> 'b
(*@ v = find h k
      ensures match data h k with Nil -> false | Cons r _ -> v = r
      raises  Not_found -> data h k = Nil *)

val length : ('a, 'b) t -> int
(*@ n = length h
      ensures n = sum (dom h) (fun k -> length (data h k)) *)
