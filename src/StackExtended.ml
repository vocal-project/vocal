
module Make (M : sig
  type 'a t
  val pop: 'a t -> 'a
  val top: 'a t -> 'a
  val is_empty: 'a t -> bool
end) = struct

  exception Empty

  let pop_exn s =
    if M.is_empty s then raise Empty;
    M.pop s

  let top_exn s =
    if M.is_empty s then raise Empty;
    M.top s

  let pop_opt s =
    if M.is_empty s then None else Some (M.pop s)

  let top_opt s =
    if M.is_empty s then raise Empty else Some (M.top s)

end

