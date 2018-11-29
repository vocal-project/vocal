(**************************************************************************)
(*                                                                        *)
(*  VOCaL -- A Verified OCaml Library                                     *)
(*                                                                        *)
(*  Copyright (c) 2018 The VOCaL Project                                  *)
(*                                                                        *)
(*  This software is free software, distributed under the MIT license     *)
(*  (as described in file LICENSE enclosed).                              *)
(**************************************************************************)

module type HashedType = sig
  type t
  (*@ predicate E (x: t) (y: t) *)
  (*@ axiom Erefl : forall x: t. E x x *)
  (*@ axiom Esym  : forall x y: t. E x y -> E y x *)
  (*@ axiom Etrans: forall x y z: t. E x y -> E y z -> E x z *)
  val equal: t -> t -> bool
  (*@ b = equal x y
      ensures b <-> E x y *)
  (* function H (x: t) : integer *)
  (* axiom compatibility: forall x y: t. E x y -> H x = H y *)
  val hash: t -> int
  (*@ h = hash x
      ensures h = H x *)
end

module Make (K : HashedType) : sig

  type key = K.t

  type 'a table
  (*@ ephemeral *)
  (*@ mutable model dom : key set *)
  (*@ invariant irredundant: forall x y: key. mem x dom -> mem y dom -> K.E x y -> x = y *)
  (*@ mutable model view: key -> 'a list *)
  (*@ invariant forall k: key. not (mem k dom) -> view k = [] *)

  type 'a t = 'a table

  val create: int -> 'a t
  (*@ h = create n
    requires n >= 0
    ensures  forall k: key. view h k = [] *)

  val clear: 'a t -> unit
  (*@ clear h
    modifies h
    ensures  forall k: key. view h k = [] *)

  val reset: 'a t -> unit
  (*@ reset h
    modifies h
    ensures  forall k: key. view h k = [] *)

  val copy: 'a t -> 'a t
  (*@ h2 = copy h1 h
    ensures  forall k: key. view h2 k = view h1 k *)

  (*@ function pop (h: 'a t) : integer =
    sum (fun k -> length (view h k)) (dom h) *)

  val population: 'a t -> int
  (*@ n = population h
    ensures n = pop h *)

  val length: 'a t -> int
  (*@ n = population h
    ensures n = pop h *)

  val iter: (key -> 'a -> unit) -> 'a t -> unit

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  type statistics = {
    num_bindings: int;
    num_buckets: int;
    max_bucket_length: int;
    bucket_histogram: int array
  }

  val stats: 'a t -> statistics

  val add: 'a t -> key -> 'a -> unit
  (*@ add h k v
    modifies h
    ensures  forall k': key.
             view h k = if K.E k' k then v :: old (view h k')
                        else old (view h k') *)

  (*@ function tail (l: 'a list) : 'a list =
        match l with [] -> [] | _ :: s -> s end *)

  val remove: 'a t -> key -> unit
  (*@ remove h k
    modifies h
    ensures  forall k': key.
             view h k = if K.E k' k then tail (old (view h k'))
                        else old (view h k') *)

  val find: 'a t -> key -> 'a option
  (*@ r = find h k
    ensures r = match view h k with [] -> None | x :: _ -> Some x end *)

  val find_all: 'a t -> key -> 'a list
  (*@ l = find_all h k
    ensures l = view h k *)

  val replace: 'a t -> key -> 'a -> unit
  (*@ replace h k v
    modifies h
    ensures  forall k': key.
             view h k = if K.E k' k then v :: tail (old (view h k))
                        else old (view h k') *)

  val mem: 'a t -> key -> bool
  (*@ b = mem h k
    ensures b <-> view h k <> [] *)

  val to_seq: 'a t -> (key * 'a) Seq.t

end
