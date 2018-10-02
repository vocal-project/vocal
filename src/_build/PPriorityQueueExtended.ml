
module AddLength(X: PPriorityQueueSig.S) = struct

  type elt = X.elt
  type t = { length: int; queue: X.t }

  let empty = { length = 0; queue = X.empty }
  let length q = q.length
  let is_empty q = X.is_empty q.queue
  let add q x = { length = q.length + 1; queue = X.add q.queue x }
  let top q = X.top q.queue
  let pop q =
    let n = q.length in
    let x, q = X.pop q.queue in x, { length = n - 1; queue = q }
  let remove_top q = { length = q.length - 1; queue = X.remove_top q.queue }

  let iter q f = X.iter q.queue f
  let fold q f acc = X.fold q.queue f acc

end

module AddOptExn(X: sig
  type elt
  type t
  val is_empty: t -> bool
  val top: t -> elt
  val pop: t -> elt * t
  val remove_top: t -> t
end) = struct

  exception Empty

  let top_exn q =
    if X.is_empty q then raise Empty;
    X.top q
  let pop_exn q =
    if X.is_empty q then raise Empty;
    X.pop q
  let remove_top_exn q =
    if X.is_empty q then raise Empty;
    X.remove_top q

  let top_opt q =
    if X.is_empty q then None else Some (X.top q)
  let pop_opt q =
    if X.is_empty q then None else Some (X.pop q)

end
