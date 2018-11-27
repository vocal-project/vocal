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
  val equal: t -> t -> bool
  val hash: t -> int
end

module Make (K : HashedType) : sig

  type key = K.t

  type 'a table

  type 'a t = 'a table

  val create: int -> 'a t

  val clear: 'a t -> unit

  val reset: 'a t -> unit

  val copy: 'a t -> 'a t

  val population: 'a t -> int

  val length: 'a t -> int

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

  val remove: 'a t -> key -> unit

  val find: 'a t -> key -> 'a option

  val find_all: 'a t -> key -> 'a list

  val replace: 'a t -> key -> 'a -> unit

  val mem: 'a t -> key -> bool

  val cascade: 'a t -> (key * 'a) Seq.t

end
