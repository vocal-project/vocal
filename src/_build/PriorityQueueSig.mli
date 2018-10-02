
(** Ephemeral priority queues *)

(*@ import relations *)
(*@ import fmultiset *)

module type S = sig

  type elt

  type t
    (*@ ephemeral *) (* stateful interface *)
    (*@ model (!): elt fmultiset *)

  val create: unit -> t
    (*@ r = create ()
          ensures !r = {} *)    (*? fresh r .. comment faire ? *)

  val is_empty: t -> bool
    (*@ r = is_empty q
          ensures r <-> !q = {} *)

  val length: t -> int
    (*@ r = length q
          ensures r = card !q *)

  val clear: t -> unit
    (*@ clear q
          modifies q
          ensures !q = {} *)

  val add: t -> elt -> unit
    (*@ add q x [E]
          requires !q = E
          modifies q
          ensures  !q = {x} `u` E *)
    (*@ add q x
          modifies q
          ensures  !q = {x} `u` (old !q) *)

    (*? modifies q to q' *)

  (*@ constant minimum: t -> elt *)
  (*@ axiom: forall t, !t <> {} -> minimum_elt (minimum t) !t *)

  val top: t -> elt
    (*@ r = top q
          requires !q <> {}
          ensures  r = minimum !q *)

(* plus là 
  val top_opt: t -> elt option
    (*@ r = top_opt q
          ensures match !r with None   -> !q = {}
                              | Some x -> !q <> {} /\ x = minimum q *)
    (*?   ensures r = if !q = {} then None else Some (minimum q) *)

  exception Empty

  val top_exn: t -> elt
    (*@ r = top q
          ensures !q <> {} /\ !r = minimum !q
          raises Empty: !q = {} *)
    (*@ derived spec, when the heap is not empty
        r = top q
          requires !q <> {}
          ensures  !r = minimum !q *)            (*? plusieurs specs *)

*)

(*
  val top_def: elt -> t -> elt
    (*@ r = top_def d q
          ensures r = if !q = {} then d else minimum q *)
*)

  val remove_top: t -> unit
    (*@ remove_top q
          modifies q
          ensures {minimum (old q)} `u` !q = old !q

          
          (* raises Empty: !q = {} *) *)

  val pop: t -> elt
    (*@ m = pop q
          modifies q
          ensures m = minimum (old !q) /\ {m} `u` !q = old !q
          raises Empty: !q = {} *)

(*
  val to_list: t -> elt list
    (*@ l = to_list q
          ensures List.bag_of l = !q *)
*)

  (*@ constant enumerated (s: elt seq) (q: t) := bagof s `sub` !q *)
  (*@ constant completed  (s: elt seq) (q: t) := bagof s = !q     *)

  val fold: t -> ('a -> elt -> 'a) -> 'a -> 'a
    (*@ fold_spec enumerated completed *)

  val iter: (elt -> unit) -> t -> unit
    (*@ iter_spec enumerated completed *)

(*
  module Cursor: Cursor.S with type elt = elt and type t = t
    (*@ cursor_spec enumerated completed *)
*)

(*
  val to_stream: t -> elt stream
    (*@ stream_spec enumerated completed *)
*)

  (*? si on veut être déterministe *)
  (*@ constant to_seq: t -> elt seq *)
  (*@ constant enumerated (s: elt seq) (q: t) := prefix s (to_seq !q) *)
  (*@ constant completed  (s: elt seq) (q: t) := s = to_seq !q        *)

end



module type Mergeable = sig

  type t

  val merge: t -> t -> unit
    (*@ merge q1 q2
          requires disjoint q1 q2
          modifies q1, q2
          ensures  !q1 = {}
          ensures  !q2 = old !q1 `u` old !q2 *)
    (*@ merge q1 q2
          requires    disjoint q1 q2
          modifies    q1, q2
          invalidates q1
          ensures     !q2 = old !q1 `u` old !q2 *)

end

module type Splittable = sig

  type t

  val split: t -> int -> t
    (*@ r = split q n
          requires 0 <= n <= card !q
          modifies q
          ensures  disjoint q r      (*? *)
          ensures  card !r = n
          ensures  !q `u` !r = old !q *)


(*
    r1 = split q (size q)    !q = {}   !r1 = old !q
    r2 = split q 0
*)

end
