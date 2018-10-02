
(** Persistent priority queues *)

(*? portée des variables logiques i.e. est qu'une valeur de programme
    nommée "le" cache le prédicat "le" *)

(*@ import relations *)
(*@ import fmultiset *)

module type S = sig

  type elt

  type t
    (*@ model (!): elt fmultiset *)   (*? ou une relation *)

  (*@ constant (!): t -> elt fmultiset *)   (*? alternative *)

  val empty: t
    (*@ r = empty
          ensures !r = {} *)            (*? result si r pas donné *)

  val is_empty: t -> bool
    (*@ r = is_empty q
          ensures r <-> !q = {} *)

  val length: t -> int
    (*@ r = length q
          ensures r = card !q *)        (*? ou # mais pas |_| *)
                                        (*? pourquoi pas !r *)

  val add: t -> elt -> t
    (*@ r = add q x
          ensures !r = {x} `u` !q *)      (*? + ++ \U etc. *)

  (*@ constant minimum: t -> elt *)
  (*@ axiom minimum_spec: forall t, !t <> {} -> minimum_elt (minimum t) !t *)

  val top: t -> elt
    (*@ r = top q
          requires !q <> {}
          ensures  result = minimum q *)

  val remove_top: t -> t
    (*@ r = remove_top q
          requires !q <> {}
          ensures {minimum q} `u` !r = !q
    *)
          (* raises Empty: !q = {} *)

  val pop: t -> elt * t
    (*@ (m, r) = pop q
          requires !q <> {}
          ensures m = minimum q /\ {m} `u` !r = !q


          ---raises Empty: !q = {} *)

(*
  val to_list: t -> elt list
    (*@ l = to_list q
          ensures List.bag_of l = !q *)
*)

  (*@ constant enumerated (s: elt seq) (q: t) := bagof s `sub` !q *)
  (*@ constant completed  (s: elt seq) (q: t) := bagof s = !q     *)

  val fold: t -> ('a -> elt -> 'a) -> 'a -> 'a
    (*@ fold_spec enumerated completed *)

  val iter: t -> (elt -> unit) -> unit
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


    (*@ r = fold f q acc0 [I]
          requires (y = f x acc [l1]
                      requires bagof (l1 ++ [x]) `sub` !q /\ I l1 acc
                      ensures  I (l1 ++ [x]) y)
          requires I [] acc0
          ensures  exists l, List.bag_of l = !q /\ I l r *)

    (*@ fold f q acc ~ List.fold_left f acc ?l,
          where bagof l = !q *)


module type PopTopExnOpt = sig

  type elt

  type t

  exception Empty

  val top_exn: t -> elt
    (*@ r = top q
          ensures !q <> {} /\ !r = minimum !q
          raises Empty: !q = {} *)
    (*@ derived spec, when the heap is not empty
        r = top q
          requires !q <> {}
          ensures  !r = minimum !q *)            (*? plusieurs specs *)

  val pop_exn: t -> elt * t
  val remove_top_exn: t -> t

  val top_opt: t -> elt option
    (*@ r = top_opt q
          ensures match !r with None   -> !q = {}
                              | Some x -> !q <> {} /\ x = minimum q *)
    (*?   ensures r = if !q = {} then None else Some (minimum q) *)

  val pop_opt: t -> (elt * t) option

(* FIXME: is this really useful?

  val top_def: t -> elt -> elt
    (*@ r = top_def q d
          ensures r = if !q = {} then d else minimum q *)
*)

end
