
module Make(X: ComparableType.S) : sig

  include PriorityQueueSig.S with type elt = X.t

  val create: ?capacity:int -> dummy:elt -> t

  (* include PriorityQueue.Mergeable with type elt = X.t and type t = t *)
end

