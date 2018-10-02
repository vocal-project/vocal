
module type ClearLength = sig

  type 'a t

  val clear: 'a t -> unit
  (** Discard all elements from a vector.
      This is equivalent to setting the size to 0 with [resize]. *)

  val is_empty: 'a t -> bool
  (** Return [true] if the given vector is empty, [false] otherwise. *)

  val length: 'a t -> int
  (** Return the length (number of elements) of the given vector.
      Note: the number of memory words occupiedby the vector can be larger. *)

end

module type GetSet = sig

  type 'a t

  val get: 'a t -> int -> 'a
  (** [Vector.get a n] returns the element number [n] of vector [a].
      The first element has number [0]. The last element has number
      [Vector.length a - 1].

      Raise [Invalid_argument "Vector.get"]
      if [n] is outside the range [0] to [Vector.length a - 1]. *)

  val set: 'a t -> int -> 'a -> unit
  (** [Vector.set a n x] modifies vector [a] in place, replacing
      element number [n] with [x].

      Raise [Invalid_argument "Vector.set"]
      if [n] is outside the range 0 to [Vector.length a - 1]. *)

end

module type SubFillBlit = sig

  type 'a t

val sub: 'a t -> int -> int -> 'a t
(** [Vector.sub a start len] returns a fresh vector of length [len], containing
    the elements number [start] to [start + len - 1] of vector [a]. *)

val fill : 'a t -> int -> int -> 'a -> unit
(** [Vector.fill a ofs len x] modifies the vector [a] in place,
    storing [x] in elements number [ofs] to [ofs + len - 1].

    Raise [Invalid_argument "Vector.fill"] if [ofs] and [len] do not
    designate a valid subvector of [a]. *)

val blit : 'a t -> int -> 'a t -> int -> int -> unit
(** [Vector.blit v1 o1 v2 o2 len] copies [len] elements
   from vector [v1], starting at element number [o1], to vector [v2],
   starting at element number [o2]. It works correctly even if
   [v1] and [v2] are the same vector, and the source and
   destination chunks overlap.

   Raise [Invalid_argument "Vector.blit"] if [o1] and [len] do not
   designate a valid subvector of [v1], or if [o2] and [len] do not
   designate a valid subvector of [v2]. *)

end

module type IterMap = sig

  type 'a t

val iter : 'a t -> ('a -> unit) -> unit
(** [Vector.iter f a] applies function [f] in turn to all
   the elements of [a].  It is equivalent to
   [f (get a 0); f (get a 1); ...; f (get a (Vector.length a - 1))]. *)

val map : 'a t -> ('a -> 'b) -> 'b t
(** [Vector.map f a] applies function [f] to all the elements of [a],
   and builds a fresh vector with the results returned by [f].

   Note: the dummy value of the returned vector is obtained by applying
   [f] to the dummy value of [a]. If this is not what you want,
   first create a new vector and then fill it with the value
   [f (get a 0)], [f (get a 1)], etc. *)

val copy: 'a t -> 'a t
(** [Vector.copy a] returns a copy of [a], that is, a fresh vector containing
    the same elements as [a]. *)

end

module type IteriMapi = sig

  type 'a t

val iteri : 'a t -> (int -> 'a -> unit) -> unit
(** Same as {!Vector.iter}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument. *)

val mapi : 'a t -> (int -> 'a -> 'b) -> 'b t
(** Same as {!Vector.map}, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument.

   Note: the dummy value of the returned vector is obtained by applying
   [f 0] to the dummy value of [a].  *)

end

module type Fold = sig

  type 'a t

val fold : 'b t -> ('a -> 'b -> 'a) -> 'a -> 'a
(** [fold f x a] computes
   [f (... (f (f x (get a 0)) (get a 1)) ...) (get a (n-1))],
   where [n] is the length of the vector [a]. *)

end

module type FoldLeftRight = sig

  type 'a t

val fold_left : 'b t -> ('a -> 'b -> 'a) -> 'a -> 'a
(** [Vector.fold_left f x a] computes
   [f (... (f (f x (get a 0)) (get a 1)) ...) (get a (n-1))],
   where [n] is the length of the vector [a]. *)

val fold_right : 'b t -> ('b -> 'a -> 'a) -> 'a -> 'a
(** [Vector.fold_right f a x] computes
   [f (get a 0) (f (get a 1) ( ... (f (get a (n-1)) x) ...))],
   where [n] is the length of the vector [a]. *)

end
