
module Make (M: sig
  type elt
  type t
  val top: t -> elt
  val remove_top: t -> unit
  val is_empty: t -> bool
end) = struct

  exception Empty

  let top_exn h =
    if M.is_empty h then raise Empty;
    M.top h

  let remove_top_exn h =
    if M.is_empty h then raise Empty;
    M.remove_top h

  let top_opt h =
    if M.is_empty h then None else Some (M.top h)

  let remove_top_opt h =
    if M.is_empty h then raise Empty else Some (M.remove_top h)

end

