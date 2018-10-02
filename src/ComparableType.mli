
(*@ use Order *)

module type S = sig
  type t
    (*@ duplicable *)

  (*@ function cmp: t -> t -> int *)

  (*@ axiom: Order.is_pre_order cmp *)

  val compare : t -> t -> int
    (*@ r = compare x y
          ensures r = cmp x y *)
end
