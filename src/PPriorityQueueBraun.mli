
(** Purely applicative priority queues,
    implemented with Braun trees *)

module Make(X: ComparableType.S) : sig

  include PPriorityQueueSig.S with type elt = X.t

  val empty: t

end
